import math
import random
from collections import namedtuple

Term = namedtuple('Term', ['index', 'target'])


def is_clause_satisfied_by(clause, vars):
    return any(vars[term.index] == term.target for term in clause)


def all_clauses_satisfied_by(clauses, vars):
    return all(is_clause_satisfied_by(clause, vars) for clause in clauses)


def decimate(vars, clause, p):
    if not is_clause_satisfied_by(clause, vars):
        for term in clause:
            if random.random() < p:
                vars[term.index] ^= True


def perturb_until_satisfied(var_count, clauses, p):
    vars = [random.random() < 0.5 for _ in range(var_count)]
    n = 0
    while not all_clauses_satisfied_by(clauses, vars):
        n += 1
        for clause in clauses:
            decimate(vars, clause, p)
        if n % 100 == 0:
            print(n)
            print(''.join('T' if e else '_' for e in vars))
    print(n)
    print(''.join('T' if e else '_' for e in vars))


def evil_3sat_instance(n):
    YA = lambda i: Term(i, True)
    no = lambda i: Term(i, False)
    return [
        # Seed: first three variables are false
        (no(0), YA(1), YA(2)),
        (no(0), YA(1), no(2)),
        (no(0), no(1), YA(2)),
        (no(0), no(1), no(2)),
        (YA(0), no(1), no(2)),
        (YA(0), no(1), YA(2)),
        (YA(0), YA(1), no(2)),
    ] + [
        # Chaining mechanism: a variable must be false if the two before it are false
        (YA(i+1), YA(i+2), no(i+3)) for i in range(n-3)
    ] + [
        # Reset mechanism:
        # 1. Every variable tends to become true if a,b is broken
        # 2. Any true variable tends to break a,b
        # 3. These tendencies are statistically stronger than the tendency to fix any one breakage
        clause
        for k in range(n-3)
        for clause in [
            (no(0), no(1), YA(k+3)),
            (no(0), YA(1), YA(k+3)),
            (YA(0), no(1), YA(k+3))
        ]
    ]

def print_clauses(clauses):
    for clause in clauses:
        print(' or '.join(('' if term.target else '!') + chr(ord('a') + term.index) for term in clause))

N = 8
p = math.sin(math.pi/4)**2
all_must_be_false_clauses = evil_3sat_instance(N)
print_clauses(all_must_be_false_clauses)
if not all_clauses_satisfied_by(all_must_be_false_clauses, [False]*N):
    raise AssertionError("Expected all-false to be a solution.")

perturb_until_satisfied(N, all_must_be_false_clauses, p)
print("DONE")
