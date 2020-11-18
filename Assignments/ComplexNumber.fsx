/// Since the assignment text explicitly says to only upload one file, the
/// signature-file's content is outlined below:
///
///
/// //Filename: ComplexNumber.fsi
///
///module ComplexNumber
///[<Sealed>]
///type ComplexNumber
///val (.+) : ComplexNumber -> ComplexNumber -> ComplexNumber
///val (.-) : ComplexNumber -> ComplexNumber -> ComplexNumber
///val (./) : ComplexNumber -> ComplexNumber -> ComplexNumber
///val (.*) : ComplexNumber -> ComplexNumber -> ComplexNumber
///
///
/// This signature file should have a corresponding implementation file ComplexNumber.fs
/// with the following contents (the `module` declaration at the top should be
/// commented in):
///
/// 
/// //Filename: Complexnumber.fs
/// 
///module ComplexNumber
///type ComplexNumber = V of float * float
///let (.+) (V(aa, ab)) (V(ba, bb)) = V(aa + ba, ab + bb )
///let (.-) (V(aa, ab)) (V(ba, bb)) = V(aa, ab) .+ V(-ba,-bb )
///let (.*) (V(aa, ab)) (V(ba, bb)) = V( aa * ba - ab * bb, aa * bb + ab * ba )
///let (./) (V(aa, ab)) (V(ba, bb)) =
///    let d = ba ** 2. + bb ** 2.
///    V(aa, ab) .* V(ba / d, -bb / d)
