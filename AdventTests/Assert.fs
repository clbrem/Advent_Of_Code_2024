namespace MyAssert

type Assert<'T>() =
    inherit Xunit.Assert() with
    static member IsTrue fails condition =
        Assert.True (condition, fails)
    static member FailWith format (item: 'T) =
        Assert.Fail(sprintf format item)
    static member EqualTo (expected: 'T) (actual: 'T) =
        Assert.Equal(expected, actual)
    static member Pass  =
        Assert.True true
    static member IsNone: 'T option -> unit =
        Option.isNone >> Assert<'T>.IsTrue "Expected None but got Some"
    static member IsSome: 'T option -> unit =
        Option.isSome >> Assert<'T>.IsTrue "Expected Some but got None"
    static member Some (test: 'T -> unit):  'T option -> unit =
        function
        | Some v -> test v
        | None -> Assert.Fail("Expected Some but got None")
    

