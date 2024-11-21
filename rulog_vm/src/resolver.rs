use rulog_core::types::ast::{Predicate, Query, Term};

use crate::environment::Environment;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct QuerySolution {
    pub env: Environment,
}

pub struct QuerySolver {
    pub query: Query,
    pub clauses: Vec<(Predicate, Vec<Predicate>)>,
    state: Vec<(Predicate, Environment)>,
    index: usize,
}

impl QuerySolver {
    pub fn new(rules: Vec<(Predicate, Vec<Predicate>)>, query: Query) -> QuerySolver {
        let initial_state = query
            .predicates
            .iter()
            .map(|predicate| (predicate.clone(), Environment::default()))
            .collect();

        QuerySolver {
            clauses: rules,
            query,
            index: 0,
            state: initial_state,
        }
    }
}

impl Iterator for QuerySolver {
    type Item = QuerySolution;

    fn next(&mut self) -> Option<Self::Item> {
        while !self.state.is_empty() {
            if self.index >= self.clauses.len() {
                self.index = 0;
                self.state.pop();
                continue;
            }

            let (goal, env) = self.state.last().unwrap().clone();
            let clause = self.clauses[self.index].clone();
            self.index += 1;

            if let Some(new_env) = self.try_unify_and_expand(&goal, &env, &clause) {
                if self.state.last().unwrap().1 == new_env {
                    self.state.pop();
                }
                return Some(QuerySolution { env: new_env });
            }
        }

        None
    }
}

impl QuerySolver {
    fn try_unify_and_expand(
        &mut self,
        goal: &Predicate,
        env: &Environment,
        clause: &(Predicate, Vec<Predicate>),
    ) -> Option<Environment> {
        // Check if the goal can be unified with the head of the clause.
        if goal.name != clause.0.name {
            return None;
        }

        // Attempt unification of the terms of the goal and the clause.
        if let Some(new_env) = unify_terms(&goal.terms, &clause.0.terms) {
            // Compose the new environment with the existing one.
            let new_env = compose(env, &new_env);

            // If the clause has a body, we need to expand the state with the new sub-goals.
            if !clause.1.is_empty() {
                let new_goals = clause.1.iter().map(|predicate| {
                    (
                        Predicate {
                            name: predicate.name.clone(),
                            terms: apply_env_terms(&predicate.terms, &new_env),
                        },
                        new_env.clone(),
                    )
                });

                // Extend the state with the new goals in reverse order.
                self.state.extend(new_goals.rev());
            }

            // If the clause is a fact (no body), or after adding new goals for a rule,
            // return the new environment.
            return Some(new_env);
        }

        // If goal and clause head match exactly, return the current environment.
        if goal == &clause.0 {
            return Some(env.clone());
        }

        None
    }
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
        Term::List(terms) => Term::List(terms.iter().map(|t| apply_env(t, env)).collect()),
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

fn unify(term1: &Term, term2: &Term) -> Option<Environment> {
    let mut env = Environment::default();
    if unify_helper(term1, term2, &mut env) {
        Some(env)
    } else {
        None
    }
}

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
            ]),
            &Term::List(vec![Term::Integer(1), Term::Integer(2)]),
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
