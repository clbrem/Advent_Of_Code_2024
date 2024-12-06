namespace MyAssert

type Assert<'T>() =
    inherit Xunit.Assert() with
    static member FailWith format (item: 'T) =
        Assert.Fail(sprintf format item)
    static member EqualTo (expected: 'T) (actual: 'T) =
        Assert.Equal(expected, actual)
        
    

