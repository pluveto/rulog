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

    fn prune_choice_points(&mut self, threshold: usize) {
        self.stack
            .retain(|state| state.choice_point_id <= threshold);
    }

    fn next_choice_point(&mut self) -> usize {
        let id = self.next_choice_point_id;
        self.next_choice_point_id += 1;
        id
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
