namespace Microsoft.Research.Liquid
open System.Collections.Generic

module WaltersAlgorithmSimulator =
    open System

    // Generic 3-SAT utility methods
    type Term = {index: int; target: bool}
    type Clause = Clause of Term * Term * Term
    let clauseSatisfiedBy (Clause(a,b,c)) (vars:bool list) =
        [a;b;c]
        |> List.map (fun term -> vars.[term.index] = term.target)
        |> List.fold (fun a b -> a || b) false
    let allClausesSatisfiedBy clauses (vars:bool list) =
        clauses
        |> List.map (fun clause -> clauseSatisfiedBy clause vars)
        |> List.fold (fun a b -> a && b) true
    let clausesToString clauses =
        let vars = "abcdefghijklmnopqrstuvwxyz"
        String.Join(
            "\n",
            clauses
            |> List.map (fun (Clause(a,b,c)) ->
                String.Join(
                    " or ",
                    [a;b;c] |> List.map (fun c ->
                        (if c.target then "" else "!") + vars.[c.index].ToString()))))

    // Problem generator.
    let evil3SatInstance varCount =
        let no i = {index = i; target = false}
        let YA i = {index = i; target = true}

        // Seeding. First three variables must be false.
        let seed = [
            Clause(no 0, YA 1, YA 2);
            Clause(no 0, YA 1, no 2);
            Clause(no 0, no 1, YA 2);
            Clause(no 0, no 1, no 2);
            Clause(YA 0, no 1, no 2);
            Clause(YA 0, no 1, YA 2);
            Clause(YA 0, YA 1, no 2);
        ]

        // Chaining. A variable must be false if the two before it are false.
        let chain =
            seq { 0 .. (varCount-4) }
            |> Seq.map (fun i -> Clause(YA(i+1), YA(i+2), no(i+3)))
            |> List.ofSeq

        // Reset mechanism.
        // 1. Every variable tends to become true if a,b is broken
        // 2. Any true variable tends to break a,b
        // 3. These tendencies are statistically stronger than the tendency to fix any one breakage
        let reset =
            seq { 0 .. (varCount-4) }
            |> Seq.map (fun i ->
                [
                    Clause(no(0), no(1), YA(i+3));
                    Clause(no(0), YA(1), YA(i+3));
                    Clause(YA(0), no(1), YA(i+3));
                ])
            |> Seq.concat
            |> List.ofSeq

        List.concat [seed; chain; reset]

    // Perturb-if-not-satisfied-er.
    let decimate (power1:int) (power2:int) (qs:Qubits) (Clause(A, B, C)) =
        let CCCNot = Operations.Cgate Operations.CCNOT
        let CPerturb = Operations.Cgate (fun reg ->
            // Turn the Z^(2^-power1 + 2^-power2) gate into an X rotation via surrounding Hadamards
            Operations.H reg
            Operations.R -power1 reg
            Operations.R -power2 reg
            Operations.H reg)

        let scratch = qs.[qs.Length-1]
        let qA = qs.[A.index]
        let qB = qs.[B.index]
        let qC = qs.[C.index]

        // '3-OR' the three variables into the scratch qubit
        if A.target then Operations.X [qA]
        if B.target then Operations.X [qB]
        if C.target then Operations.X [qC]
        CCCNot [qA; qB; qC; scratch]
        if A.target then Operations.X [qA]
        if B.target then Operations.X [qB]
        if C.target then Operations.X [qC]

        // When clause isn't sarget = b perturb the involved qubits
        CPerturb [scratch; qA]
        CPerturb [scratch; qB]
        CPerturb [scratch; qC]

        // Discard the scratch qubit
        Operations.H [scratch] // <-- not necessary
        Operations.M [scratch] // <-- could be done right after the CCCNot
        Operations.Reset Bit.Zero [scratch]
       
    // Iterated per-clause perturb-if-not-satisfied-er.
    let waltersDecimationAlgorithm clauses steps (vars:Qubits) =
        // Init into uniform superposition of all assignments (skipping scratch bit).
        for var in vars.[0..vars.Length-2] do
            Operations.H [var]

        // Decimate every clause again and again until we hit the given number of repetitions.
        let rand = new Random()
        for i in 1..steps do
            // The decimation power starts high (90 deg) and scales down slowly over time.
            let power1 = -3

            for clause in clauses do
                let power2 = if rand.NextDouble() < 0.5 then -2 else -3
                decimate power1 power2 vars clause
            
            // Debug output.
            let probs = String.Join(" ", (vars |> List.map (fun e -> String.Format("{0:0}%", e.Prob1*100.0).PadLeft(4))))
            let degs1 = 360.0*(Math.Pow(2.0, float(power1)) + Math.Pow(2.0, float(power1) + float(-1)))
            let degs2 = 360.0*(Math.Pow(2.0, float(power1)) + Math.Pow(2.0, float(power1) + float(-2)))
            printf "Iter %d, Angles %0.1f vs %0.1f degs, Qubit Probs %s\n" i degs1 degs2 probs

        // Measure result.
        for var in vars do
            Operations.M [var]

    [<EntryPoint>]
    let Main _ =
        // Choose parameters
        let varCount = 8
        let steps = 1000
        let clauses = evil3SatInstance varCount
        printf "Clauses:\n%s\n\n" (clausesToString clauses)

        // Apply the algorithm.
        printf "Running algorithm...\n"
        let ket = Ket (varCount+1)
        waltersDecimationAlgorithm clauses steps ket.Qubits
        let assignment = ket.Qubits |> List.map (fun e -> e.Bit.v = 1)
        let success = allClausesSatisfiedBy clauses assignment

        // Show the result.
        let bitString = String.Join("", assignment |> List.map (fun e -> if e then "T" else "f"))
        printf "\n\nMeasured result: %s\n" bitString
        printf "Satisfies clauses: %A\n" success
        0 //Program ended successfully.
