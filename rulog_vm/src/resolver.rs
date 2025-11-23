use std::cmp::Ordering;

use rulog_core::types::ast::{Predicate, Query, Term};

use crate::environment::Environment;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct QuerySolution {
    pub env: Environment,
}

#[derive(Clone)]
struct ExecutionState {
    goals: Vec<Predicate>,
    env: Environment,
    choice_point_id: usize,
    cut_parent_id: usize,
}

pub struct QuerySolver {
    pub query: Query,
    pub clauses: Vec<(Predicate, Vec<Predicate>)>,
    stack: Vec<ExecutionState>,
    next_choice_point_id: usize,
    fresh_var_counter: usize,
    epsilon_per_call: usize,
}

impl QuerySolver {
    pub fn new(rules: Vec<(Predicate, Vec<Predicate>)>, query: Query) -> QuerySolver {
        let mut initial_goals = query.predicates.clone();
        initial_goals.reverse();
        let initial_state = ExecutionState {
            goals: initial_goals,
            env: Environment::default(),
            choice_point_id: 0,
            cut_parent_id: 0,
        };

        QuerySolver {
            clauses: rules,
            query,
            stack: vec![initial_state],
            next_choice_point_id: 1,
            fresh_var_counter: 0,
            epsilon_per_call: 2,
        }
    }
}

impl Iterator for QuerySolver {
    type Item = QuerySolution;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(mut state) = self.stack.pop() {
            if state.goals.is_empty() {
                return Some(QuerySolution { env: state.env });
            }

            if let Some(goal) = state.goals.pop() {
                self.expand_goal(state, goal);
            }
        }

        None
    }
}

impl QuerySolver {
    fn expand_goal(&mut self, state: ExecutionState, goal: Predicate) {
        if self.handle_builtin(state.clone(), &goal) {
            return;
        }

        let mut new_state_data = Vec::new();

        for clause in &self.clauses {
            if goal.name != clause.0.name {
                continue;
            }

            if let Some(new_env) = try_unify_and_expand(&goal, &state.env, clause) {
                let mut next_goals = state.goals.clone();
                if !clause.1.is_empty() {
                    for predicate in clause.1.iter().rev() {
                        next_goals
                            .push(apply_env_predicate(predicate, &new_env));
                    }
                }
                new_state_data.push((next_goals, new_env));
            }
        }

        let mut states_with_ids = Vec::new();
        for (goals, env) in new_state_data {
            let choice_point_id = self.next_choice_point();
            states_with_ids.push((choice_point_id, goals, env));
        }

        for (choice_point_id, goals, env) in states_with_ids.into_iter().rev() {
            self.stack.push(ExecutionState {
                goals,
                env,
                choice_point_id,
                cut_parent_id: state.choice_point_id,
            });
        }
    }

    fn handle_builtin(&mut self, state: ExecutionState, goal: &Predicate) -> bool {
        match goal.name.as_str() {
            "!" if goal.terms.is_empty() => {
                self.prune_choice_points(state.cut_parent_id);
                self.stack.push(state);
                true
            }
            "fail" if goal.terms.is_empty() => true,
            "true" if goal.terms.is_empty() => {
                self.stack.push(state);
                true
            }
            ";" if goal.terms.len() == 2 => {
                let left = goal_sequence_from_term(&goal.terms[0]);
                let right = goal_sequence_from_term(&goal.terms[1]);
                match (left, right) {
                    (Some(left_goals), Some(right_goals)) => {
                        let left_choice_point = self.next_choice_point();
                        let right_choice_point = self.next_choice_point();

                        let mut right_state_goals = state.goals.clone();
                        append_goal_sequence(&mut right_state_goals, right_goals);
                        self.stack.push(ExecutionState {
                            goals: right_state_goals,
                            env: state.env.clone(),
                            choice_point_id: right_choice_point,
                            cut_parent_id: state.cut_parent_id,
                        });

                        let mut left_state_goals = state.goals;
                        append_goal_sequence(&mut left_state_goals, left_goals);
                        self.stack.push(ExecutionState {
                            goals: left_state_goals,
                            env: state.env,
                            choice_point_id: left_choice_point,
                            cut_parent_id: state.cut_parent_id,
                        });
                        true
                    }
                    _ => false,
                }
            }
            "->" if goal.terms.len() == 2 => {
                if let (Some(cond_goals), Some(then_goals)) = (
                    goal_sequence_from_term(&goal.terms[0]),
                    goal_sequence_from_term(&goal.terms[1]),
                ) {
                    let mut new_goals = state.goals;
                    append_goal_sequence(&mut new_goals, then_goals);
                    new_goals.push(Predicate {
                        name: "!".to_string(),
                        terms: vec![],
                    });
                    append_goal_sequence(&mut new_goals, cond_goals);
                    self.stack.push(ExecutionState {
                        goals: new_goals,
                        env: state.env,
                        choice_point_id: state.choice_point_id,
                        cut_parent_id: state.cut_parent_id,
                    });
                    true
                } else {
                    false
                }
            }
            "is" if goal.terms.len() == 2 => {
                match evaluate_numeric_term(&goal.terms[1], &state.env) {
                    Ok(result) => {
                        let rhs_term = result.into_term();
                        let lhs = apply_env(&goal.terms[0], &state.env);
                        let mut delta_env = Environment::default();
                        if unify_helper(&lhs, &rhs_term, &mut delta_env) {
                            let new_env = compose(&state.env, &delta_env);
                            self.stack.push(ExecutionState {
                                goals: state.goals,
                                env: new_env,
                                choice_point_id: state.choice_point_id,
                                cut_parent_id: state.cut_parent_id,
                            });
                            true
                        } else {
                            false
                        }
                    }
                    Err(err) => {
                        log::debug!("is/2 evaluation failed: {:?}", err);
                        false
                    }
                }
            }
            "<" | ">" | ">=" | "=<" | "<=" | "=:=" | "=\\=" if goal.terms.len() == 2 => {
                self.handle_comparison(state, goal)
            }
            "var" if goal.terms.len() == 1 => {
                let term = apply_env(&goal.terms[0], &state.env);
                if matches!(term, Term::Variable(_)) {
                    self.stack.push(state);
                    true
                } else {
                    false
                }
            }
            "nonvar" if goal.terms.len() == 1 => {
                let term = apply_env(&goal.terms[0], &state.env);
                if !matches!(term, Term::Variable(_)) {
                    self.stack.push(state);
                    true
                } else {
                    false
                }
            }
            "atom" if goal.terms.len() == 1 => {
                let term = apply_env(&goal.terms[0], &state.env);
                if matches!(term, Term::Atom(_)) {
                    self.stack.push(state);
                    true
                } else {
                    false
                }
            }
            "integer" if goal.terms.len() == 1 => {
                let term = apply_env(&goal.terms[0], &state.env);
                if matches!(term, Term::Integer(_)) {
                    self.stack.push(state);
                    true
                } else {
                    false
                }
            }
            "float" if goal.terms.len() == 1 => {
                let term = apply_env(&goal.terms[0], &state.env);
                if matches!(term, Term::Float(_)) {
                    self.stack.push(state);
                    true
                } else {
                    false
                }
            }
            "compound" if goal.terms.len() == 1 => {
                let term = apply_env(&goal.terms[0], &state.env);
                if matches!(term, Term::Structure(_, _) | Term::List(_, _)) {
                    self.stack.push(state);
                    true
                } else {
                    false
                }
            }
            "functor" if goal.terms.len() == 3 => self.handle_functor(state, goal),
            "arg" if goal.terms.len() == 3 => self.handle_arg(state, goal),
            "=.." if goal.terms.len() == 2 => self.handle_univ(state, goal),
            "\\+" | "not" if goal.terms.len() == 1 => {
                if let Some(negated_goals) = goal_sequence_from_term(&goal.terms[0]) {
                    let failure_choice_point = self.next_choice_point();
                    let success_choice_point = self.next_choice_point();

                    let success_goals = state.goals.clone();
                    let mut failure_goals = state.goals;
                    failure_goals.push(Predicate {
                        name: "fail".to_string(),
                        terms: vec![],
                    });
                    failure_goals.push(Predicate {
                        name: "!".to_string(),
                        terms: vec![],
                    });
                    append_goal_sequence(&mut failure_goals, negated_goals);

                    let failure_env = state.env;
                    let success_env = failure_env.clone();
                    let failure_state = ExecutionState {
                        goals: failure_goals,
                        env: failure_env,
                        choice_point_id: failure_choice_point,
                        cut_parent_id: state.cut_parent_id,
                    };
                    let success_state = ExecutionState {
                        goals: success_goals,
                        env: success_env,
                        choice_point_id: success_choice_point,
                        cut_parent_id: state.cut_parent_id,
                    };
                    self.stack.push(success_state);
                    self.stack.push(failure_state);
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn handle_comparison(&mut self, state: ExecutionState, goal: &Predicate) -> bool {
        use std::cmp::Ordering;
        let left = match evaluate_numeric_term(&goal.terms[0], &state.env) {
            Ok(value) => value,
            Err(err) => {
                log::debug!("comparison left eval failed: {:?}", err);
                return false;
            }
        };
        let right = match evaluate_numeric_term(&goal.terms[1], &state.env) {
            Ok(value) => value,
            Err(err) => {
                log::debug!("comparison right eval failed: {:?}", err);
                return false;
            }
        };
        let ordering = compare_numeric_values(left, right);
        let success = match goal.name.as_str() {
            "<" => ordering == Ordering::Less,
            ">" => ordering == Ordering::Greater,
            ">=" => ordering == Ordering::Greater || ordering == Ordering::Equal,
            "=<" | "<=" => ordering == Ordering::Less || ordering == Ordering::Equal,
            "=:=" => ordering == Ordering::Equal,
            "=\\=" => ordering != Ordering::Equal,
            _ => false,
        };
        if success {
            self.stack.push(state);
        }
        success
    }

    fn handle_functor(&mut self, state: ExecutionState, goal: &Predicate) -> bool {
        let term = apply_env(&goal.terms[0], &state.env);
        let name_term = apply_env(&goal.terms[1], &state.env);
        let arity_term = apply_env(&goal.terms[2], &state.env);

        match term {
            Term::Variable(var_name) => {
                let functor_name = match name_term {
                    Term::Atom(name) => name,
                    _ => return false,
                };
                let arity = match arity_term {
                    Term::Integer(n) if n >= 0 => n as usize,
                    _ => return false,
                };
                let args: Vec<Term> = if arity == 0 {
                    Vec::new()
                } else {
                    (0..arity).map(|_| self.fresh_variable()).collect()
                };
                let constructed = match build_term_from_name_and_args(&functor_name, &args) {
                    Some(term) => term,
                    None => return false,
                };
                let mut delta = Environment::default();
                delta.bind(var_name, constructed);
                let new_env = compose(&state.env, &delta);
                self.stack.push(ExecutionState {
                    goals: state.goals,
                    env: new_env,
                    choice_point_id: state.choice_point_id,
                    cut_parent_id: state.cut_parent_id,
                });
                true
            }
            _ => {
                let (name, args) = match decompose_functor_term(&term) {
                    Some(parts) => parts,
                    None => return false,
                };
                let mut delta = Environment::default();
                if !unify_helper(
                    &name_term,
                    &Term::Atom(name),
                    &mut delta,
                ) {
                    return false;
                }
                if !unify_helper(
                    &arity_term,
                    &Term::Integer(args.len() as i64),
                    &mut delta,
                ) {
                    return false;
                }
                let new_env = compose(&state.env, &delta);
                self.stack.push(ExecutionState {
                    goals: state.goals,
                    env: new_env,
                    choice_point_id: state.choice_point_id,
                    cut_parent_id: state.cut_parent_id,
                });
                true
            }
        }
    }

    fn handle_arg(&mut self, state: ExecutionState, goal: &Predicate) -> bool {
        let index = match apply_env(&goal.terms[0], &state.env) {
            Term::Integer(i) if i > 0 => i as usize,
            _ => return false,
        };
        let structure_term = apply_env(&goal.terms[1], &state.env);
        let target = apply_env(&goal.terms[2], &state.env);
        let (_name, args) = match decompose_functor_term(&structure_term) {
            Some(parts) => parts,
            None => return false,
        };
        if index == 0 || index > args.len() {
            return false;
        }
        let arg_value = apply_env(&args[index - 1], &state.env);
        let mut delta = Environment::default();
        if unify_helper(&target, &arg_value, &mut delta) {
            let new_env = compose(&state.env, &delta);
            self.stack.push(ExecutionState {
                goals: state.goals,
                env: new_env,
                choice_point_id: state.choice_point_id,
                cut_parent_id: state.cut_parent_id,
            });
            true
        } else {
            false
        }
    }

    fn handle_univ(&mut self, state: ExecutionState, goal: &Predicate) -> bool {
        let left = apply_env(&goal.terms[0], &state.env);
        let right = apply_env(&goal.terms[1], &state.env);
        match left {
            Term::Variable(var) => match right {
                Term::List(mut elements, None) if !elements.is_empty() => {
                    let functor_name = match elements.remove(0) {
                        Term::Atom(name) => name,
                        _ => return false,
                    };
                    let term = match build_term_from_name_and_args(&functor_name, &elements) {
                        Some(term) => term,
                        None => return false,
                    };
                    let mut delta = Environment::default();
                    delta.bind(var, term);
                    let new_env = compose(&state.env, &delta);
                    self.stack.push(ExecutionState {
                        goals: state.goals,
                        env: new_env,
                        choice_point_id: state.choice_point_id,
                        cut_parent_id: state.cut_parent_id,
                    });
                    true
                }
                _ => false,
            },
            _ => {
                let (name, args) = match decompose_functor_term(&left) {
                    Some(parts) => parts,
                    None => return false,
                };
                let list_term = functor_to_list(name, args);
                let mut delta = Environment::default();
                if unify_helper(&right, &list_term, &mut delta) {
                    let new_env = compose(&state.env, &delta);
                    self.stack.push(ExecutionState {
                        goals: state.goals,
                        env: new_env,
                        choice_point_id: state.choice_point_id,
                        cut_parent_id: state.cut_parent_id,
                    });
                    true
                } else {
                    false
                }
            }
        }
    }

    fn handle_call(&mut self, state: ExecutionState, goal: &Predicate) -> bool {
        if goal.terms.is_empty() {
            return false;
        }
        let callable = apply_env(&goal.terms[0], &state.env);
        let args: Vec<Term> = goal.terms[1..]
            .iter()
            .map(|t| apply_env(t, &state.env))
            .collect();
        let predicate = match callable {
            Term::Atom(name) => Predicate { name, terms: args },
            Term::Structure(name, mut terms) => {
                for arg in args {
                    terms.push(arg);
                }
                Predicate { name, terms }
            }
            _ => return false,
        };
        let mut new_goals = state.goals.clone();
        new_goals.push(predicate);
        self.stack.push(ExecutionState {
            goals: new_goals,
            env: state.env,
            choice_point_id: state.choice_point_id,
            cut_parent_id: state.cut_parent_id,
        });
        true
    }


    fn prune_choice_points(&mut self, threshold: usize) {
        self.stack
            .retain(|state| state.choice_point_id <= threshold);
    }

    fn next_choice_point(&mut self) -> usize {
        let id = self.next_choice_point_id;
        self.next_choice_point_id += 1;
        id
    }

    fn fresh_variable(&mut self) -> Term {
        let name = format!("_G{}", self.fresh_var_counter);
        self.fresh_var_counter += 1;
        Term::Variable(name)
    }

}

fn try_unify_and_expand(
    goal: &Predicate,
    env: &Environment,
    clause: &(Predicate, Vec<Predicate>),
) -> Option<Environment> {
    if let Some(new_env) = unify_terms(&goal.terms, &clause.0.terms) {
        let new_env = compose(env, &new_env);
        return Some(new_env);
    }

    if goal == &clause.0 {
        return Some(env.clone());
    }

    None
}

#[test]
fn test_query_solver_no_var() {
    let rules = vec![(
        Predicate {
            name: "parent".to_string(),
            terms: vec![Term::Atom("a".to_string())],
        },
        vec![],
    )];
    let query = Query {
        predicates: vec![Predicate {
            name: "parent".to_string(),
            terms: vec![Term::Atom("a".to_string())],
        }],
    };

    let mut query_solver = QuerySolver::new(rules, query);
    let next_solution = query_solver.next();
    assert_eq!(
        next_solution,
        Some(QuerySolution {
            env: Environment::default()
        })
    );
    assert_eq!(query_solver.next(), None);
}

#[test]
fn test_query_solver_no_var_true() {
    let rules = vec![(
        Predicate {
            name: "parent".to_string(),
            terms: vec![Term::Atom("tom".to_string()), Term::Atom("liz".to_string())],
        },
        vec![],
    )];
    let query = Query {
        predicates: vec![Predicate {
            name: "parent".to_string(),
            terms: vec![Term::Atom("tom".to_string()), Term::Atom("liz".to_string())],
        }],
    };

    let mut query_solver = QuerySolver::new(rules, query);
    let next_solution = query_solver.next();
    assert_eq!(
        next_solution,
        Some(QuerySolution {
            env: Environment::default()
        })
    );
    assert_eq!(query_solver.next(), None);
}

#[test]
fn test_cut_prunes_choice_points() {
    /*
        f(a).
        f(b).
        g(X) :- f(X), !.
        ?- g(X). % expect X = a only
    */
    let rules = vec![
        (
            Predicate {
                name: "f".to_string(),
                terms: vec![Term::Atom("a".to_string())],
            },
            vec![],
        ),
        (
            Predicate {
                name: "f".to_string(),
                terms: vec![Term::Atom("b".to_string())],
            },
            vec![],
        ),
        (
            Predicate {
                name: "g".to_string(),
                terms: vec![Term::Variable("X".to_string())],
            },
            vec![
                Predicate {
                    name: "f".to_string(),
                    terms: vec![Term::Variable("X".to_string())],
                },
                Predicate {
                    name: "!".to_string(),
                    terms: vec![],
                },
            ],
        ),
    ];
    let query = Query {
        predicates: vec![Predicate {
            name: "g".to_string(),
            terms: vec![Term::Variable("X".to_string())],
        }],
    };

    let mut solver = QuerySolver::new(rules, query);
    let solution = solver.next();
    assert_eq!(
        solution,
        Some(QuerySolution {
            env: [("X".to_string(), Term::Atom("a".to_string()))]
                .iter()
                .cloned()
                .collect()
        })
    );
    assert_eq!(solver.next(), None);
}

#[test]
fn test_fail_builtin() {
    let rules = vec![];
    let query = Query {
        predicates: vec![Predicate {
            name: "fail".to_string(),
            terms: vec![],
        }],
    };
    let mut solver = QuerySolver::new(rules, query);
    assert_eq!(solver.next(), None);
}

#[test]
fn test_query_solver_no_match() {
    let rules = vec![(
        Predicate {
            name: "parent".to_string(),
            terms: vec![Term::Atom("tom".to_string()), Term::Atom("liz".to_string())],
        },
        vec![],
    )];
    let query = Query {
        predicates: vec![Predicate {
            name: "parent".to_string(),
            terms: vec![Term::Atom("tom".to_string()), Term::Atom("bob".to_string())],
        }],
    };

    let mut query_solver = QuerySolver::new(rules, query);
    assert_eq!(query_solver.next(), None);
}

#[test]
fn test_query_solver_with_var() {
    /*
        f(a).
        ?- f(X). % expect X = a
    */
    let rules = vec![(
        Predicate {
            name: "f".to_string(),
            terms: vec![Term::Atom("a".to_string())],
        },
        vec![],
    )];
    let query = Query {
        predicates: vec![Predicate {
            name: "f".to_string(),
            terms: vec![Term::Variable("X".to_string())],
        }],
    };

    let mut query_solver = QuerySolver::new(rules, query);
    let next_solution = query_solver.next();
    assert_eq!(
        next_solution,
        Some(QuerySolution {
            env: [("X".to_string(), Term::Atom("a".to_string()))]
                .iter()
                .cloned()
                .collect()
        })
    );
    assert_eq!(query_solver.next(), None);
}

#[test]
fn test_query_solver() {
    /*
       parent(tom, liz).
       parent(tom, bob).
       ?- parent(tom, X).
    */
    let rules = vec![
        (
            Predicate {
                name: "parent".to_string(),
                terms: vec![Term::Atom("tom".to_string()), Term::Atom("liz".to_string())],
            },
            vec![],
        ),
        (
            Predicate {
                name: "parent".to_string(),
                terms: vec![Term::Atom("tom".to_string()), Term::Atom("bob".to_string())],
            },
            vec![],
        ),
    ];

    let query = Query {
        predicates: vec![Predicate {
            name: "parent".to_string(),
            terms: vec![
                Term::Atom("tom".to_string()),
                Term::Variable("X".to_string()),
            ],
        }],
    };

    let mut query_solver = QuerySolver::new(rules, query);
    let next_solution = query_solver.next();
    assert_eq!(
        next_solution,
        Some(QuerySolution {
            env: [("X".to_string(), Term::Atom("liz".to_string()))]
                .iter()
                .cloned()
                .collect()
        })
    );
    let next_solution = query_solver.next();
    assert_eq!(
        next_solution,
        Some(QuerySolution {
            env: [("X".to_string(), Term::Atom("bob".to_string()))]
                .iter()
                .cloned()
                .collect()
        })
    );
}

#[test]
fn test_disjunction_operator() {
    let rules = vec![
        (
            Predicate {
                name: "left".to_string(),
                terms: vec![Term::Atom("a".to_string())],
            },
            vec![],
        ),
        (
            Predicate {
                name: "right".to_string(),
                terms: vec![Term::Atom("b".to_string())],
            },
            vec![],
        ),
    ];
    let query = Query {
        predicates: vec![Predicate {
            name: ";".to_string(),
            terms: vec![
                Term::Structure(
                    "left".to_string(),
                    vec![Term::Variable("X".to_string())],
                ),
                Term::Structure(
                    "right".to_string(),
                    vec![Term::Variable("X".to_string())],
                ),
            ],
        }],
    };

    let mut solver = QuerySolver::new(rules, query);
    let first = solver.next().unwrap();
    assert_eq!(
        first.env,
        [("X".to_string(), Term::Atom("a".to_string()))]
            .iter()
            .cloned()
            .collect()
    );
    let second = solver.next().unwrap();
    assert_eq!(
        second.env,
        [("X".to_string(), Term::Atom("b".to_string()))]
            .iter()
            .cloned()
            .collect()
    );
    assert_eq!(solver.next(), None);
}

#[test]
fn test_if_then_else_operator() {
    let rules = vec![
        (
            Predicate {
                name: "condition".to_string(),
                terms: vec![],
            },
            vec![],
        ),
        (
            Predicate {
                name: "then_value".to_string(),
                terms: vec![Term::Atom("one".to_string())],
            },
            vec![],
        ),
        (
            Predicate {
                name: "else_value".to_string(),
                terms: vec![Term::Atom("two".to_string())],
            },
            vec![],
        ),
    ];
    let query = Query {
        predicates: vec![Predicate {
            name: ";".to_string(),
            terms: vec![
                Term::Structure(
                    "->".to_string(),
                    vec![
                        Term::Structure("condition".to_string(), vec![]),
                        Term::Structure(
                            "then_value".to_string(),
                            vec![Term::Variable("X".to_string())],
                        ),
                    ],
                ),
                Term::Structure(
                    "else_value".to_string(),
                    vec![Term::Variable("X".to_string())],
                ),
            ],
        }],
    };

    let mut solver = QuerySolver::new(rules, query);
    let result = solver.next().unwrap();
    assert_eq!(
        result.env,
        [("X".to_string(), Term::Atom("one".to_string()))]
            .iter()
            .cloned()
            .collect()
    );
    assert_eq!(solver.next(), None);
}

#[test]
fn test_negation_operator() {
    let rules = vec![(
        Predicate {
            name: "fact".to_string(),
            terms: vec![Term::Atom("a".to_string())],
        },
        vec![],
    )];

    let query_success = Query {
        predicates: vec![Predicate {
            name: "\\+".to_string(),
            terms: vec![Term::Structure(
                "fact".to_string(),
                vec![Term::Atom("b".to_string())],
            )],
        }],
    };

    let mut solver = QuerySolver::new(rules.clone(), query_success);
    let result = solver.next();
    assert!(result.is_some());
    assert_eq!(solver.next(), None);

    let query_failure = Query {
        predicates: vec![Predicate {
            name: "\\+".to_string(),
            terms: vec![Term::Structure(
                "fact".to_string(),
                vec![Term::Atom("a".to_string())],
            )],
        }],
    };

    let mut solver_fail = QuerySolver::new(rules, query_failure);
    assert_eq!(solver_fail.next(), None);
}

#[test]
fn test_is_builtin_assigns_value() {
    let rules = vec![];
    let query = Query {
        predicates: vec![Predicate {
            name: "is".to_string(),
            terms: vec![
                Term::Variable("X".to_string()),
                Term::Structure(
                    "+".to_string(),
                    vec![Term::Integer(1), Term::Integer(2)],
                ),
            ],
        }],
    };

    let mut solver = QuerySolver::new(rules, query);
    let solution = solver.next().unwrap();
    assert_eq!(
        solution.env,
        [("X".to_string(), Term::Integer(3))]
            .into_iter()
            .collect()
    );
    assert_eq!(solver.next(), None);
}

#[test]
fn test_is_builtin_with_existing_binding() {
    let rules = vec![
        (
            Predicate {
                name: "start".to_string(),
                terms: vec![Term::Variable("X".to_string())],
            },
            vec![Predicate {
                name: "is".to_string(),
                terms: vec![
                    Term::Variable("X".to_string()),
                    Term::Structure(
                        "-".to_string(),
                        vec![Term::Integer(10), Term::Integer(3)],
                    ),
                ],
            }],
        ),
    ];
    let query = Query {
        predicates: vec![Predicate {
            name: "start".to_string(),
            terms: vec![Term::Variable("R".to_string())],
        }],
    };

    let mut solver = QuerySolver::new(rules, query);
    let solution = solver.next().unwrap();
    assert_eq!(
        apply_env(&Term::Variable("R".to_string()), &solution.env),
        Term::Integer(7)
    );
    assert_eq!(solver.next(), None);
}

#[test]
fn test_less_than_builtin_succeeds() {
    let rules = vec![];
    let query = Query {
        predicates: vec![Predicate {
            name: "<".to_string(),
            terms: vec![Term::Integer(1), Term::Integer(2)],
        }],
    };
    let mut solver = QuerySolver::new(rules, query);
    assert!(solver.next().is_some());
    assert_eq!(solver.next(), None);
}

#[test]
fn test_greater_equal_builtin_fails() {
    let rules = vec![];
    let query = Query {
        predicates: vec![Predicate {
            name: ">=".to_string(),
            terms: vec![Term::Integer(2), Term::Integer(5)],
        }],
    };
    let mut solver = QuerySolver::new(rules, query);
    assert_eq!(solver.next(), None);
}

#[test]
fn test_var_and_nonvar_builtins() {
    let rules = vec![];
    let query_var = Query {
        predicates: vec![Predicate {
            name: "var".to_string(),
            terms: vec![Term::Variable("X".to_string())],
        }],
    };
    let mut solver_var = QuerySolver::new(rules.clone(), query_var);
    assert!(solver_var.next().is_some());

    let query_nonvar = Query {
        predicates: vec![Predicate {
            name: "nonvar".to_string(),
            terms: vec![Term::Atom("hello".to_string())],
        }],
    };
    let mut solver_nonvar = QuerySolver::new(rules, query_nonvar);
    assert!(solver_nonvar.next().is_some());

    let query_atom = Query {
        predicates: vec![Predicate {
            name: "atom".to_string(),
            terms: vec![Term::Atom("world".to_string())],
        }],
    };
    let mut solver_atom = QuerySolver::new(vec![], query_atom);
    assert!(solver_atom.next().is_some());

    let query_integer = Query {
        predicates: vec![Predicate {
            name: "integer".to_string(),
            terms: vec![Term::Integer(42)],
        }],
    };
    assert!(QuerySolver::new(vec![], query_integer).next().is_some());

    let query_float = Query {
        predicates: vec![Predicate {
            name: "float".to_string(),
            terms: vec![Term::Float(3.14.into())],
        }],
    };
    assert!(QuerySolver::new(vec![], query_float).next().is_some());

    let query_compound = Query {
        predicates: vec![Predicate {
            name: "compound".to_string(),
            terms: vec![Term::Structure(
                "foo".to_string(),
                vec![Term::Integer(1)],
            )],
        }],
    };
    assert!(QuerySolver::new(vec![], query_compound).next().is_some());
}

#[test]
fn test_functor_decompose_and_construct() {
    let rules = vec![];
    let query = Query {
        predicates: vec![Predicate {
            name: "functor".to_string(),
            terms: vec![
                Term::Structure(
                    "foo".to_string(),
                    vec![Term::Atom("a".to_string()), Term::Atom("b".to_string())],
                ),
                Term::Variable("F".to_string()),
                Term::Variable("N".to_string()),
            ],
        }],
    };
    let mut solver = QuerySolver::new(rules, query);
    let solution = solver.next().unwrap();
    assert_eq!(
        apply_env(&Term::Variable("F".to_string()), &solution.env),
        Term::Atom("foo".to_string())
    );
    assert_eq!(
        apply_env(&Term::Variable("N".to_string()), &solution.env),
        Term::Integer(2)
    );

    let construct_query = Query {
        predicates: vec![Predicate {
            name: "functor".to_string(),
            terms: vec![
                Term::Variable("T".to_string()),
                Term::Atom("baz".to_string()),
                Term::Integer(0),
            ],
        }],
    };
    let mut construct_solver = QuerySolver::new(vec![], construct_query);
    let constructed = construct_solver.next().unwrap();
    assert_eq!(
        apply_env(&Term::Variable("T".to_string()), &constructed.env),
        Term::Atom("baz".to_string())
    );
}

#[test]
fn test_arg_builtin() {
    let rules = vec![];
    let query = Query {
        predicates: vec![Predicate {
            name: "arg".to_string(),
            terms: vec![
                Term::Integer(2),
                Term::Structure(
                    "pair".to_string(),
                    vec![Term::Atom("a".to_string()), Term::Atom("b".to_string())],
                ),
                Term::Variable("X".to_string()),
            ],
        }],
    };
    let mut solver = QuerySolver::new(rules, query);
    let solution = solver.next().unwrap();
    assert_eq!(
        apply_env(&Term::Variable("X".to_string()), &solution.env),
        Term::Atom("b".to_string())
    );
}

#[test]
fn test_univ_builtin() {
    let rules = vec![];
    let query = Query {
        predicates: vec![Predicate {
            name: "=..".to_string(),
            terms: vec![
                Term::Structure(
                    "pair".to_string(),
                    vec![Term::Atom("a".to_string()), Term::Atom("b".to_string())],
                ),
                Term::Variable("L".to_string()),
            ],
        }],
    };
    let mut solver = QuerySolver::new(rules, query);
    let solution = solver.next().unwrap();
    assert_eq!(
        apply_env(&Term::Variable("L".to_string()), &solution.env),
        Term::List(
            vec![
                Term::Atom("pair".to_string()),
                Term::Atom("a".to_string()),
                Term::Atom("b".to_string())
            ],
            None
        )
    );

    let build_query = Query {
        predicates: vec![Predicate {
            name: "=..".to_string(),
            terms: vec![
                Term::Variable("T".to_string()),
                Term::List(
                    vec![
                        Term::Atom("pair".to_string()),
                        Term::Atom("x".to_string()),
                        Term::Atom("y".to_string()),
                    ],
                    None,
                ),
            ],
        }],
    };
    let mut build_solver = QuerySolver::new(vec![], build_query);
    let built = build_solver.next().unwrap();
    assert_eq!(
        apply_env(&Term::Variable("T".to_string()), &built.env),
        Term::Structure(
            "pair".to_string(),
            vec![Term::Atom("x".to_string()), Term::Atom("y".to_string())]
        )
    );
}

/// Composes two environments.
fn compose(env1: &Environment, env2: &Environment) -> Environment {
    let mut env = env1.clone();
    for (var, term) in env2.bindings.iter() {
        env.bind(var.clone(), apply_env(term, &env));
    }
    env
}

#[test]
fn test_compose() {
    let mut env1 = Environment::default();
    env1.bind("X".to_string(), Term::Integer(1));
    env1.bind("Y".to_string(), Term::Integer(2));
    env1.bind("Z".to_string(), Term::Integer(3));
    let mut env2 = Environment::default();
    env2.bind("X".to_string(), Term::Integer(4));
    env2.bind("Y".to_string(), Term::Integer(5));
    env2.bind("W".to_string(), Term::Integer(6));
    let env = compose(&env1, &env2);
    assert_eq!(
        env.bindings,
        [
            ("X".to_string(), Term::Integer(4)),
            ("Y".to_string(), Term::Integer(5)),
            ("Z".to_string(), Term::Integer(3)),
            ("W".to_string(), Term::Integer(6))
        ]
        .iter()
        .cloned()
        .collect()
    );
}

/// Applies an environment to a term.
fn apply_env(term: &Term, env: &Environment) -> Term {
    match term {
        Term::Variable(var) => {
            if let Some(binding) = env.lookup(var) {
                apply_env(binding, env)
            } else {
                term.clone()
            }
        }
        Term::List(terms, tail) => Term::List(
            terms.iter().map(|t| apply_env(t, env)).collect(),
            tail.as_ref()
                .map(|t| Box::new(apply_env(t, env))),
        ),
        Term::Structure(name, terms) => Term::Structure(
            name.clone(),
            terms.iter().map(|t| apply_env(t, env)).collect(),
        ),
        _ => term.clone(),
    }
}

#[test]
fn test_apply_env() {
    let mut env = Environment::default();
    env.bind("X".to_string(), Term::Integer(1));
    env.bind("Y".to_string(), Term::Integer(2));
    env.bind("Z".to_string(), Term::Integer(3));
    assert_eq!(
        apply_env(
            &Term::Structure(
                "foo".to_string(),
                vec![
                    Term::Variable("X".to_string()),
                    Term::Variable("Y".to_string()),
                    Term::Variable("Z".to_string())
                ]
            ),
            &env
        ),
        Term::Structure(
            "foo".to_string(),
            vec![Term::Integer(1), Term::Integer(2), Term::Integer(3)]
        )
    );
}

fn apply_env_terms(terms: &[Term], env: &Environment) -> Vec<Term> {
    terms.iter().map(|t| apply_env(t, env)).collect()
}

fn apply_env_predicate(predicate: &Predicate, env: &Environment) -> Predicate {
    Predicate {
        name: predicate.name.clone(),
        terms: apply_env_terms(&predicate.terms, env),
    }
}

fn empty_list_term() -> Term {
    Term::List(vec![], None)
}

// fn unify(term1: &Term, term2: &Term) -> Option<Environment> {
//     let mut env = Environment::default();
//     if unify_helper(term1, term2, &mut env) {
//         Some(env)
//     } else {
//         None
//     }
// }

fn unify_terms(terms1: &[Term], terms2: &[Term]) -> Option<Environment> {
    let mut env = Environment::default();
    if terms1.len() != terms2.len() {
        return None;
    }
    for (term1, term2) in terms1.iter().zip(terms2.iter()) {
        if !unify_helper(term1, term2, &mut env) {
            return None;
        }
    }
    Some(env)
}

fn unify_helper(term1: &Term, term2: &Term, env: &mut Environment) -> bool {
    match (term1, term2) {
        // simple case: the terms are equal
        (Term::Atom(a1), Term::Atom(a2)) if a1 == a2 => true,
        (Term::Integer(i1), Term::Integer(i2)) if i1 == i2 => true,
        (Term::Float(f1), Term::Float(f2)) if f1 == f2 => true,
        (Term::String(s1), Term::String(s2)) if s1 == s2 => true,
        (Term::Variable(v1), Term::Variable(v2)) if v1 == v2 => true,
        // if one of the terms is a variable, bind it to the other term
        (Term::Variable(v), t) | (t, Term::Variable(v)) => {
            // if the variable is already bound, unify the bound term with the other term
            if let Some(binding) = env.lookup(v) {
                let binding = binding.clone();
                unify_helper(&binding, t, env)
            } else {
                env.bind(v.clone(), t.clone());
                true
            }
        }
        // if both terms are lists and have the same length, unify the pairs of items
        (Term::List(items1, tail1), Term::List(items2, tail2)) => {
            unify_list_terms(items1, tail1, items2, tail2, env)
        }
        // if both terms are list structures and have the same name and arity, unify the pairs of items
        (Term::Structure(name1, terms1), Term::Structure(name2, terms2))
            if name1 == name2 && terms1.len() == terms2.len() =>
        {
            for (item1, item2) in terms1.iter().zip(terms2.iter()) {
                if !unify_helper(item1, item2, env) {
                    return false;
                }
            }
            true
        }
        // otherwise, the terms cannot be unified
        _ => false,
    }
}

fn unify_list_terms(
    items1: &[Term],
    tail1: &Option<Box<Term>>,
    items2: &[Term],
    tail2: &Option<Box<Term>>,
    env: &mut Environment,
) -> bool {
    let mut index = 0;
    while index < items1.len() && index < items2.len() {
        if !unify_helper(&items1[index], &items2[index], env) {
            return false;
        }
        index += 1;
    }

    if index < items1.len() {
        let remainder = Term::List(items1[index..].to_vec(), tail1.clone());
        return match tail2 {
            Some(tail) => unify_helper(&remainder, tail, env),
            None => unify_helper(&remainder, &empty_list_term(), env),
        };
    }

    if index < items2.len() {
        let remainder = Term::List(items2[index..].to_vec(), tail2.clone());
        return match tail1 {
            Some(tail) => unify_helper(tail, &remainder, env),
            None => unify_helper(&empty_list_term(), &remainder, env),
        };
    }

    match (tail1, tail2) {
        (Some(t1), Some(t2)) => unify_helper(t1, t2, env),
        (Some(t1), None) => unify_helper(t1, &empty_list_term(), env),
        (None, Some(t2)) => unify_helper(&empty_list_term(), t2, env),
        (None, None) => true,
    }
}

fn goal_sequence_from_term(term: &Term) -> Option<Vec<Predicate>> {
    match term {
        Term::Structure(name, terms) if name == "," && terms.len() == 2 => {
            let mut left = goal_sequence_from_term(&terms[0])?;
            left.extend(goal_sequence_from_term(&terms[1])?);
            Some(left)
        }
        _ => predicate_from_term(term).map(|predicate| vec![predicate]),
    }
}

fn predicate_from_term(term: &Term) -> Option<Predicate> {
    match term {
        Term::Structure(name, terms) => Some(Predicate {
            name: name.clone(),
            terms: terms.clone(),
        }),
        Term::Atom(name) => Some(Predicate {
            name: name.clone(),
            terms: vec![],
        }),
        _ => None,
    }
}

fn append_goal_sequence(target: &mut Vec<Predicate>, sequence: Vec<Predicate>) {
    for goal in sequence.into_iter().rev() {
        target.push(goal);
    }
}

fn normalize_list_term(term: Term) -> Term {
    match term {
        Term::Atom(name) if name == "[]" => Term::List(vec![], None),
        Term::Structure(name, mut args) if name == "." && args.len() == 2 => {
            let head = args.remove(0);
            let tail = normalize_list_term(args.remove(0));
            match tail {
        Term::List(items, tail_tail) => {
            let mut new_items = vec![head];
            new_items.extend(items.into_iter());
            Term::List(new_items, tail_tail)
        }
                other => Term::List(vec![head], Some(Box::new(other))),
            }
        }
        other => other,
    }
}

fn build_list_from_cons(head: Term, tail: Term) -> Term {
    match normalize_list_term(tail) {
        Term::List(items, tail_tail) => {
            let mut new_items = vec![head];
            new_items.extend(items.into_iter());
            Term::List(new_items, tail_tail)
        }
        other => Term::List(vec![head], Some(Box::new(other))),
    }
}

fn decompose_functor_term(term: &Term) -> Option<(String, Vec<Term>)> {
    match term {
        Term::Structure(name, args) => Some((name.clone(), args.clone())),
        Term::Atom(name) => Some((name.clone(), vec![])),
        Term::List(items, tail) => {
            if items.is_empty() && tail.is_none() {
                Some(("[]".to_string(), vec![]))
            } else if let Some(head) = items.first().cloned() {
                let rest = if items.len() > 1 {
                    Term::List(items[1..].to_vec(), tail.clone())
                } else {
                    tail.as_ref()
                        .map(|t| *t.clone())
                        .unwrap_or(Term::List(vec![], None))
                };
                Some((".".to_string(), vec![head, rest]))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn build_term_from_name_and_args(name: &str, args: &[Term]) -> Option<Term> {
    if name == "[]" {
        if args.is_empty() {
            Some(Term::List(vec![], None))
        } else {
            None
        }
    } else if args.is_empty() {
        Some(Term::Atom(name.to_string()))
    } else {
        Some(Term::Structure(name.to_string(), args.to_vec()))
    }
}

fn functor_to_list(name: String, args: Vec<Term>) -> Term {
    let mut elements = Vec::with_capacity(args.len() + 1);
    elements.push(Term::Atom(name));
    elements.extend(args);
    Term::List(elements, None)
}

#[allow(dead_code)]
#[derive(Debug)]
enum ArithmeticError {
    UnsupportedOperator(String),
    InvalidOperand(Term),
    ArityMismatch(String),
    UnboundVariable(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum NumericValue {
    Integer(i64),
    Float(f64),
}

impl NumericValue {
    fn into_term(self) -> Term {
        match self {
            NumericValue::Integer(i) => Term::Integer(i),
            NumericValue::Float(f) => Term::Float(f.into()),
        }
    }

    fn as_f64(self) -> f64 {
        match self {
            NumericValue::Integer(i) => i as f64,
            NumericValue::Float(f) => f,
        }
    }
}

fn evaluate_numeric_term(term: &Term, env: &Environment) -> Result<NumericValue, ArithmeticError> {
    let resolved = apply_env(term, env);
    evaluate_numeric(&resolved, env)
}

fn evaluate_numeric(term: &Term, env: &Environment) -> Result<NumericValue, ArithmeticError> {
    match term {
        Term::Integer(i) => Ok(NumericValue::Integer(*i)),
        Term::Float(f) => Ok(NumericValue::Float(f.0)),
        Term::Variable(name) => Err(ArithmeticError::UnboundVariable(name.clone())),
        Term::Structure(name, terms) => match name.as_str() {
            "+" => evaluate_binary_op(terms, env, |l, r| add_values(l, r)),
            "-" => {
                if terms.len() == 1 {
                    evaluate_numeric(&terms[0], env).map(negate_value)
                } else {
                    evaluate_binary_op(terms, env, |l, r| subtract_values(l, r))
                }
            }
            "*" => evaluate_binary_op(terms, env, |l, r| multiply_values(l, r)),
            "/" => evaluate_binary_op(terms, env, |l, r| divide_values(l, r)),
            _ => Err(ArithmeticError::UnsupportedOperator(name.clone())),
        },
        _ => Err(ArithmeticError::InvalidOperand(term.clone())),
    }
}

fn evaluate_binary_op<F>(
    terms: &[Term],
    env: &Environment,
    op: F,
) -> Result<NumericValue, ArithmeticError>
where
    F: Fn(NumericValue, NumericValue) -> NumericValue,
{
    if terms.len() != 2 {
        return Err(ArithmeticError::ArityMismatch(format!("{}", terms.len())));
    }
    let left = evaluate_numeric(&terms[0], env)?;
    let right = evaluate_numeric(&terms[1], env)?;
    Ok(op(left, right))
}

fn add_values(left: NumericValue, right: NumericValue) -> NumericValue {
    match (left, right) {
        (NumericValue::Integer(l), NumericValue::Integer(r)) => NumericValue::Integer(l + r),
        (l, r) => NumericValue::Float(l.as_f64() + r.as_f64()),
    }
}

fn subtract_values(left: NumericValue, right: NumericValue) -> NumericValue {
    match (left, right) {
        (NumericValue::Integer(l), NumericValue::Integer(r)) => NumericValue::Integer(l - r),
        (l, r) => NumericValue::Float(l.as_f64() - r.as_f64()),
    }
}

fn multiply_values(left: NumericValue, right: NumericValue) -> NumericValue {
    match (left, right) {
        (NumericValue::Integer(l), NumericValue::Integer(r)) => NumericValue::Integer(l * r),
        (l, r) => NumericValue::Float(l.as_f64() * r.as_f64()),
    }
}

fn divide_values(left: NumericValue, right: NumericValue) -> NumericValue {
    NumericValue::Float(left.as_f64() / right.as_f64())
}

fn negate_value(value: NumericValue) -> NumericValue {
    match value {
        NumericValue::Integer(i) => NumericValue::Integer(-i),
        NumericValue::Float(f) => NumericValue::Float(-f),
    }
}

fn compare_numeric_values(left: NumericValue, right: NumericValue) -> Ordering {
    match (left, right) {
        (NumericValue::Integer(l), NumericValue::Integer(r)) => l.cmp(&r),
        (l, r) => l
            .as_f64()
            .partial_cmp(&r.as_f64())
            .unwrap_or(Ordering::Equal),
    }
}

#[test]
fn test_unify_helper() {
    let mut env = Environment::default();
    assert_eq!(
        unify_helper(
            &Term::Structure(
                "foo".to_string(),
                vec![
                    Term::Variable("X".to_string()),
                    Term::Variable("Y".to_string()),
                    Term::Variable("Z".to_string())
                ]
            ),
            &Term::Structure(
                "foo".to_string(),
                vec![Term::Integer(1), Term::Integer(2), Term::Integer(3)]
            ),
            &mut env
        ),
        true
    );
    assert_eq!(
        env.bindings,
        [
            ("X".to_string(), Term::Integer(1)),
            ("Y".to_string(), Term::Integer(2)),
            ("Z".to_string(), Term::Integer(3))
        ]
        .iter()
        .cloned()
        .collect()
    );
}
#[test]
fn test_unify_with_nested_structures() {
    let mut env = Environment::default();
    assert_eq!(
        unify_helper(
            &Term::Structure(
                "parent".to_string(),
                vec![
                    Term::Structure("person".to_string(), vec![Term::Variable("X".to_string())]),
                    Term::Structure("person".to_string(), vec![Term::Variable("Y".to_string())])
                ]
            ),
            &Term::Structure(
                "parent".to_string(),
                vec![
                    Term::Structure("person".to_string(), vec![Term::Atom("alice".to_string())]),
                    Term::Structure("person".to_string(), vec![Term::Atom("bob".to_string())])
                ]
            ),
            &mut env
        ),
        true
    );
    assert_eq!(
        env.bindings,
        [
            ("X".to_string(), Term::Atom("alice".to_string())),
            ("Y".to_string(), Term::Atom("bob".to_string()))
        ]
        .iter()
        .cloned()
        .collect()
    );
}

#[test]
fn test_unify_with_lists() {
    let mut env = Environment::default();
    assert_eq!(
        unify_helper(
            &Term::List(vec![
                Term::Variable("X".to_string()),
                Term::Variable("Y".to_string())
            ], None),
            &Term::List(vec![Term::Integer(1), Term::Integer(2)], None),
            &mut env
        ),
        true
    );
    assert_eq!(
        env.bindings,
        [
            ("X".to_string(), Term::Integer(1)),
            ("Y".to_string(), Term::Integer(2))
        ]
        .iter()
        .cloned()
        .collect()
    );
}

#[test]
fn test_unify_with_recursive_structures() {
    let mut env = Environment::default();
    assert_eq!(
        unify_helper(
            &Term::Structure(
                "node".to_string(),
                vec![
                    Term::Variable("X".to_string()),
                    Term::Structure(
                        "node".to_string(),
                        vec![
                            Term::Variable("Y".to_string()),
                            Term::Variable("Z".to_string())
                        ]
                    )
                ]
            ),
            &Term::Structure(
                "node".to_string(),
                vec![
                    Term::Integer(1),
                    Term::Structure("node".to_string(), vec![Term::Integer(2), Term::Integer(3)])
                ]
            ),
            &mut env
        ),
        true
    );
    assert_eq!(
        env.bindings,
        [
            ("X".to_string(), Term::Integer(1)),
            ("Y".to_string(), Term::Integer(2)),
            ("Z".to_string(), Term::Integer(3))
        ]
        .iter()
        .cloned()
        .collect()
    );
}

#[test]
fn test_unify_with_failure() {
    let mut env = Environment::default();
    assert_eq!(
        unify_helper(
            &Term::Structure("foo".to_string(), vec![Term::Variable("X".to_string())]),
            &Term::Structure("bar".to_string(), vec![Term::Integer(1)]),
            &mut env
        ),
        false
    );
    assert!(env.bindings.is_empty());
}

#[test]
fn test_unify_with_existing_bindings() {
    let mut env = Environment::default();
    env.bindings.insert("X".to_string(), Term::Integer(1));

    assert_eq!(
        unify_helper(
            &Term::Variable("X".to_string()),
            &Term::Integer(1),
            &mut env
        ),
        true
    );
    assert_eq!(
        env.bindings,
        [("X".to_string(), Term::Integer(1))]
            .iter()
            .cloned()
            .collect()
    );
}
