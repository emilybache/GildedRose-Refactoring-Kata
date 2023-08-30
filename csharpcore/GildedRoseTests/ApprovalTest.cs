using System;
using System.IO;
using System.Text;
using ApprovalTests;
using ApprovalTests.Reporters;
using NUnit.Framework;

namespace GildedRoseTests;

[UseReporter(typeof(DiffReporter))]
[TestFixture]
public class ApprovalTest
{
    [Test]
    public void ThirtyDays()
    {
        StringBuilder fakeOutput = new StringBuilder();
        Console.SetOut(new StringWriter(fakeOutput));
        Console.SetIn(new StringReader($"a{Environment.NewLine}"));

        TextTestFixture.Main(new string[] { });
        var output = fakeOutput.ToString();

        Approvals.Verify(output);
    }
}