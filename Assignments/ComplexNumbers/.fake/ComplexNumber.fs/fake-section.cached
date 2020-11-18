module ComplexNumber
type ComplexNumber = V of float * float

let (.+) (V(aa, ab)) (V(ba, bb)) = V(aa + ba, ab + bb )
let (.-) (V(aa, ab)) (V(ba, bb)) = V(aa, ab) .+ V(-ba,-bb )
let (.*) (V(aa, ab)) (V(ba, bb)) = V( aa * ba - ab * bb, aa * bb + ab * ba )
let (./) (V(aa, ab)) (V(ba, bb)) =
    let d = ba ** 2. + bb ** 2.
    V(aa, ab) .* V(ba / d, -bb / d)