use colored::Colorize;
use rulog_core::types::ast::{Predicate, Query, Term};

use crate::environment::Environment;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct QuerySolution {
    pub env: Environment,
}

pub struct QuerySolver {
    rules: Vec<(Predicate, Vec<Predicate>)>,
    query: Query,
    /// Stack of environments for backtracking
    /// which stores the history of variable bindings
    env_stack: Vec<Environment>,
    /// Choice points for backtracking
    /// which stores the index of the query and the environment at that point
    choice_points: Vec<(usize, Environment)>,
    /// Current index in the query
    current_index: usize,
    /// for debugging or delecting infinite loops
    __iteration_counter: usize,
    __iteration_counter_threshold: usize,
}

impl QuerySolver {
    pub fn new(rules_or_facts: Vec<(Predicate, Vec<Predicate>)>, query: Query) -> Self {
        QuerySolver {
            rules: rules_or_facts,
            query,
            // Start with an empty environment
            env_stack: vec![Environment::new()],
            choice_points: Vec::new(),
            current_index: 0,
            __iteration_counter: 0,
            __iteration_counter_threshold: std::usize::MAX / 2,
        }
    }

    pub fn set_iteration_counter_threshold(&mut self, threshold: usize) {
        self.__iteration_counter_threshold = threshold;
    }
}

impl Iterator for QuerySolver {
    type Item = QuerySolution;

    fn next(&mut self) -> Option<Self::Item> {
        log::trace!("{}", "find next solution".green());
        while self.current_index < self.query.predicates.len() {
            log::trace!(
                "current_index: {}, query: {:?}",
                self.current_index,
                self.query
            );
            self.__iteration_counter += 1;
            if self.__iteration_counter > self.__iteration_counter_threshold {
                panic!("too many iterations");
            }

            let predicate = &self.query.predicates[self.current_index];
            let mut env = self.env_stack.pop().unwrap_or_default();

            // Attempt to unify the predicate with a rule or fact
            for (head, body) in &self.rules {
                if let Some(new_env) = unify_predicate(&predicate, head) {
                    // Apply the current environment to the body of the rule
                    let body_with_env = body
                        .iter()
                        .map(|p| apply_env_predicate(p, &env))
                        .collect::<Vec<_>>();
                    // Save the current choice point for backtracking
                    self.choice_points.push((self.current_index, env.clone()));
                    log::trace!(
                        "unify {} {:?} {} {:?}; new index {:?}; {} {:?}",
                        "goal".blue(),
                        predicate,
                        "clause".blue(),
                        (head, body),
                        self.current_index,
                        "env".blue(),
                        env
                    );
                    // Update the environment with the new
                    env = compose(&env, &new_env);
                    // If the rule has a body, add it to the query stack
                    if !body_with_env.is_empty() {
                        self.query.predicates.splice(
                            self.current_index..self.current_index,
                            body_with_env.iter().cloned(),
                        );
                    }
                    self.env_stack.push(env);
                    self.current_index += 1;
                    break;
                }
            }

            // Backtrack if no rule or fact is found
            if self.env_stack.is_empty() {
                if let Some((index, saved_env)) = self.choice_points.pop() {
                    self.env_stack.push(saved_env);
                    self.current_index = index;
                } else {
                    return None; // No more solutions
                }
            }
        }

        // Extract the solution from the environment stack
        let solution = self.env_stack.pop().map(|env| QuerySolution { env });
        log::trace!(
            "got {solution_s}  {solution:?}",
            solution_s = "solution".green(),
            solution = solution
        );
        solution
    }
}
// You would need to implement the `compose`, `apply_env`, `apply_env_terms`, `unify`, `unify_terms`, and `unify_helper` functions as per your logic.

// pub struct QuerySolver {
//     /// The clauses are a list of rules or facts.
//     clauses: Vec<(Predicate, Vec<Predicate>)>,
//     /// The state is a stack of goals and environments.
//     /// When all goals are solved, the state is empty.
//     state: Vec<(Predicate, Environment)>,
//     /// Indicates the index of the current clause to try.
//     index: usize,
//     _iteration_counter: usize,
// }

// impl QuerySolver {
//     pub fn new(rules_or_facts: Vec<(Predicate, Vec<Predicate>)>, query: Query) -> QuerySolver {
//         let initial_state = query
//             .predicates
//             .iter()
//             .map(|predicate| (predicate.clone(), Environment::new()))
//             .collect();

//         log::trace!("initial state: {:?}", initial_state);

//         QuerySolver {
//             clauses: rules_or_facts,
//             index: 0,
//             state: initial_state,
//             _iteration_counter: 0,
//         }
//     }
// }

// impl Iterator for QuerySolver {
//     type Item = QuerySolution;

//     fn next(&mut self) -> Option<Self::Item> {
//         log::trace!("{}", "find next solution".green());
//         while !self.state.is_empty() {
//             // TODO: if there are a lot of clauses, we can index them by name
//             if self.index >= self.clauses.len() {
//                 self.index = 0;
//                 let poped = self.state.pop();
//                 log::trace!(
//                     "no more clauses to try, backtrace. reset index, {} and drop {:?}",
//                     "pop".red(),
//                     poped
//                 );
//                 continue;
//             }

//             let (goal, env) = self.state.last().unwrap().clone();
//             let clause = self.clauses[self.index].clone();
//             self.index += 1;

//             log::trace!(
//                 "try to unify {} {:?} \n      with {}{:?}; new index {:?}; {} {:?}",
//                 "goal".blue(),
//                 goal,
//                 "clause".blue(),
//                 clause,
//                 self.index,
//                 "env".blue(),
//                 env
//             );

//             if let Some(new_env) = self.try_unify_and_expand(&goal, &env, &clause) {
//                 if self.state.last().unwrap().1 == new_env {
//                     let poped = self.state.pop();
//                     log::trace!("no new goals, {} and drop {:?}", "pop".red(), poped);
//                 }
//                 // all goals should be solved
//                 if self.state.is_empty() {
//                     log::trace!("got solution  {:?}", new_env);
//                     return Some(QuerySolution { env: new_env });
//                 }
//                 log::trace!("new state: {:?}", self.state);
//                 self._iteration_counter += 1;
//                 if self._iteration_counter > 1000 {
//                     panic!("too many iterations");
//                 }
//             }
//         }
//         None
//     }
// }

// impl QuerySolver {
//     /// Attempts to unify the current goal with a clause.
//     ///
//     /// The clause can be a fact or a rule.
//     ///
//     /// * If the clause is a rule, it will be expanded into sub-goals.
//     ///
//     /// * If the clause is a fact, the current goal will be removed from the state.
//     ///   And the new environment will be returned.
//     fn try_unify_and_expand(
//         &mut self,
//         goal: &Predicate,
//         env: &Environment,
//         clause: &(Predicate, Vec<Predicate>),
//     ) -> Option<Environment> {
//         if goal.name != clause.0.name {
//             log::trace!(
//                 "skip clause because of name mismatch, expect {:?}, got {:?}",
//                 clause.0.name,
//                 goal.name
//             );
//             return None;
//         }

//         // Attempt unification of the terms of the goal and the clause.
//         if let Some(new_env) = unify_terms(&goal.terms, &clause.0.terms) {
//             // Compose the new environment with the existing one.
//             let new_env = compose(&env, &new_env);

//             // If the clause has a body, we need to expand the state with the new sub-goals.
//             if !clause.1.is_empty() {
//                 let new_goals = clause.1.iter().map(|predicate| {
//                     (
//                         Predicate {
//                             name: predicate.name.clone(),
//                             terms: apply_env_terms(&predicate.terms, &new_env),
//                         },
//                         new_env.clone(),
//                     )
//                 });

//                 // Remove the current goal from the state.
//                 let poped = self.state.pop();

//                 // Extend the state with the new goals in reverse order.
//                 let expanded = new_goals.rev().collect::<Vec<_>>();
//                 self.state.extend(expanded.clone());

//                 log::trace!(
//                     "conform to rule, {} {:?} {} {:?}",
//                     "replace".blue(),
//                     poped,
//                     "with".blue(),
//                     expanded
//                 );

//                 return None;
//             }

//             return Some(new_env);
//         }

//         None
//     }
// }

/// Composes two environments.
fn compose(env1: &Environment, env2: &Environment) -> Environment {
    let mut env = Environment::new();
    for (var, term) in env1.iter() {
        env.insert(var.clone(), apply_env(term, env2));
    }
    for (var, term) in env2.iter() {
        env.insert(var.clone(), apply_env(term, env1));
    }
    env
}

/// Applies an environment to a term.
fn apply_env(term: &Term, env: &Environment) -> Term {
    match term {
        Term::Variable(var) => {
            if let Some(binding) = env.get(var) {
                apply_env(binding, env)
            } else {
                term.clone()
            }
        }
        Term::List(terms) => Term::List(terms.iter().map(|t| apply_env(t, env)).collect()),
        Term::Structure(name, terms) => Term::Structure(
            name.clone(),
            terms.iter().map(|t| apply_env(t, env)).collect(),
        ),
        _ => term.clone(),
    }
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

fn unify_term(term1: &Term, term2: &Term) -> Option<Environment> {
    let mut env = Environment::new();
    if unify_helper(term1, term2, &mut env) {
        Some(env)
    } else {
        None
    }
}

fn unify_predicate(predicate1: &Predicate, predicate2: &Predicate) -> Option<Environment> {
    if predicate1.name != predicate2.name {
        return None;
    }
    unify_terms(&predicate1.terms, &predicate2.terms)
}

// attempts to unify two lists of terms by recursively comparing corresponding
// terms and building an Environment object to store variable
// If unification succeeds, it returns the Environment object; otherwise, it returns None.
fn unify_terms(terms1: &[Term], terms2: &[Term]) -> Option<Environment> {
    log::trace!("unify terms {:?} and {:?}", terms1, terms2);
    if terms1.len() != terms2.len() {
        log::trace!("skip because of length mismatch");
        return None;
    }

    let mut env = Environment::new();
    for (term1, term2) in terms1.iter().zip(terms2.iter()) {
        if !unify_helper(term1, term2, &mut env) {
            log::trace!("unification failure for {:?} and {:?}", term1, term2);
            return None;
        }
    }
    log::trace!("unification success,  {:?}", env);
    Some(env)
}

// recursively compares two terms and builds an Environment object to store variable
fn unify_helper(term1: &Term, term2: &Term, env: &mut Environment) -> bool {
    match (term1, term2) {
        // simple case: constant terms are equal
        (Term::Atom(a1), Term::Atom(a2)) if a1 == a2 => true,
        (Term::Integer(i1), Term::Integer(i2)) if i1 == i2 => true,
        (Term::Float(f1), Term::Float(f2)) if f1 == f2 => true,
        (Term::String(s1), Term::String(s2)) if s1 == s2 => true,
        (Term::Variable(_), Term::Variable(_)) => true,
        // if one of the terms is a variable, bind it to the other term
        (Term::Variable(v), t) | (t, Term::Variable(v)) => {
            // if the variable is already bound, unify the bound term with the other term
            if let Some(binding) = env.get(v) {
                let binding = binding.clone();
                return unify_helper(&binding, t, env);
            } else {
                env.insert(v.clone(), t.clone());
                return true;
            }
        }
        // if both terms are lists and have the same length, unify the pairs of items
        (Term::List(l1), Term::List(l2)) if l1.len() == l2.len() => {
            for (item1, item2) in l1.iter().zip(l2.iter()) {
                if !unify_helper(item1, item2, env) {
                    return false;
                }
            }
            true
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
#[cfg(test)]
mod test {
    use rulog_test_util::setup_logger;

    use super::*;

    #[test]
    fn test_query_solver_no_var() {
        //
        // parent(a).
        // ?- parent(a). % expect true
        //
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
                env: Environment::new()
            })
        );
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
    fn test_query_grandparent_simple() {
        setup_logger();
        /*
            parent(tom, liz).
            parent(liz, bob).

            grandparent(X_g, Y_g) :- parent(X_g, Z_g), parent(Z_g, Y_g).

            ?- grandparent(tom, X).
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
                    terms: vec![Term::Atom("liz".to_string()), Term::Atom("bob".to_string())],
                },
                vec![],
            ),
            (
                Predicate {
                    name: "grandparent".to_string(),
                    terms: vec![
                        Term::Variable("X_g".to_string()),
                        Term::Variable("Y_g".to_string()),
                    ],
                },
                vec![
                    Predicate {
                        name: "parent".to_string(),
                        terms: vec![
                            Term::Variable("X_g".to_string()),
                            Term::Variable("Z_g".to_string()),
                        ],
                    },
                    Predicate {
                        name: "parent".to_string(),
                        terms: vec![
                            Term::Variable("Z_g".to_string()),
                            Term::Variable("Y_g".to_string()),
                        ],
                    },
                ],
            ),
        ];

        let query = Query {
            predicates: vec![Predicate {
                name: "grandparent".to_string(),
                terms: vec![
                    Term::Atom("tom".to_string()),
                    Term::Variable("X".to_string()),
                ],
            }],
        };

        let mut query_solver = QuerySolver::new(rules, query);
        query_solver.set_iteration_counter_threshold(10);

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

        let next_solution = query_solver.next();
        assert_eq!(next_solution, None);
    }

    #[test]
    fn test_query_grandparent() {
        /*
            parent(tom, liz).
            parent(tom, bob).
            parent(pam, bob).
            parent(bob, ann).
            parent(bob, pat).
            parent(pat, jim).
            grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
            ?- grandparent(tom, X).
        */
        /*
            tom  pam
            / \  |
           /   \ |
         liz   bob
                / \
               /   \
              ann  pat
                   |
                   |
                  jim


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
            (
                Predicate {
                    name: "parent".to_string(),
                    terms: vec![Term::Atom("pam".to_string()), Term::Atom("bob".to_string())],
                },
                vec![],
            ),
            (
                Predicate {
                    name: "parent".to_string(),
                    terms: vec![Term::Atom("bob".to_string()), Term::Atom("ann".to_string())],
                },
                vec![],
            ),
            (
                Predicate {
                    name: "parent".to_string(),
                    terms: vec![Term::Atom("bob".to_string()), Term::Atom("pat".to_string())],
                },
                vec![],
            ),
            (
                Predicate {
                    name: "parent".to_string(),
                    terms: vec![Term::Atom("pat".to_string()), Term::Atom("jim".to_string())],
                },
                vec![],
            ),
            (
                Predicate {
                    name: "grandparent".to_string(),
                    terms: vec![
                        Term::Variable("X".to_string()),
                        Term::Variable("Y".to_string()),
                    ],
                },
                vec![
                    Predicate {
                        name: "parent".to_string(),
                        terms: vec![
                            Term::Variable("X".to_string()),
                            Term::Variable("Z".to_string()),
                        ],
                    },
                    Predicate {
                        name: "parent".to_string(),
                        terms: vec![
                            Term::Variable("Z".to_string()),
                            Term::Variable("Y".to_string()),
                        ],
                    },
                ],
            ),
        ];

        let query = Query {
            predicates: vec![Predicate {
                name: "grandparent".to_string(),
                terms: vec![
                    Term::Atom("tom".to_string()),
                    Term::Variable("X".to_string()),
                ],
            }],
        };

        let mut query_solver = QuerySolver::new(rules, query);
        query_solver.set_iteration_counter_threshold(100);
        let next_solution = query_solver.next();
        assert_eq!(
            next_solution,
            Some(QuerySolution {
                env: [("X".to_string(), Term::Atom("ann".to_string()))]
                    .iter()
                    .cloned()
                    .collect()
            })
        );

        let next_solution = query_solver.next();
        assert_eq!(
            next_solution,
            Some(QuerySolution {
                env: [("X".to_string(), Term::Atom("pat".to_string()))]
                    .iter()
                    .cloned()
                    .collect()
            })
        );

        let next_solution = query_solver.next();
        assert_eq!(next_solution, None);
    }

    #[test]
    fn test_compose() {
        let mut env1 = Environment::new();
        env1.insert("X".to_string(), Term::Integer(1));
        env1.insert("Y".to_string(), Term::Integer(2));
        env1.insert("Z".to_string(), Term::Integer(3));
        let mut env2 = Environment::new();
        env2.insert("X".to_string(), Term::Integer(4));
        env2.insert("Y".to_string(), Term::Integer(5));
        env2.insert("W".to_string(), Term::Integer(6));
        let env = compose(&env1, &env2);
        assert_eq!(
            env,
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
    #[test]
    fn test_apply_env() {
        let mut env = Environment::new();
        env.insert("X".to_string(), Term::Integer(1));
        env.insert("Y".to_string(), Term::Integer(2));
        env.insert("Z".to_string(), Term::Integer(3));
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

    #[test]
    fn test_unify_helper() {
        let mut env = Environment::new();
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
            env,
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

    // unify terms [Atom("tom"), Variable("X")] and [Variable("X"), Variable("Y")]
    #[test]
    fn test_unify_terms() {
        setup_logger();
        let mut expected_env = Environment::new();
        expected_env.insert("X".to_string(), Term::Atom("tom".to_string()));

        assert_eq!(
            unify_terms(
                &vec![
                    Term::Atom("tom".to_string()),
                    Term::Variable("X".to_string())
                ],
                &vec![
                    Term::Variable("X".to_string()),
                    Term::Variable("Y".to_string())
                ]
            ),
            Some(expected_env)
        );
    }
}
