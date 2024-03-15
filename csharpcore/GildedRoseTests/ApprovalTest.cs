using System;
using System.IO;
using System.Text;
using FluentAssertions;
using NUnit.Framework;

namespace GildedRoseTests;

public class ApprovalTest
{
    [Test]
    public void ThirtyDays()
    {
        var fakeOutput = new StringBuilder();
        Console.SetOut(new StringWriter(fakeOutput));
        Console.SetIn(new StringReader($"a{Environment.NewLine}"));

        TextTestFixture.Main(new string[] { "30" });
        var output = fakeOutput.ToString();

        var expectedOutput = File.ReadAllText(@"C:\MyFiles\Training\GildedRose-Refactoring-Kata\csharpcore\GildedRoseTests\ExpectedApprovalTestOutput_WithoutConjured.txt");
        expectedOutput.Should().Be(output);
    }
}