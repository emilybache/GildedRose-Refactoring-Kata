using ApprovalTests;
using ApprovalTests.Reporters;
using csharp.StrategyPatternExample;
using NUnit.Framework;
using System;
using System.IO;
using System.Text;

namespace csharp
{
    [TestFixture]
    [UseReporter(typeof(NUnitReporter))]
    public class ApprovalTest
    {
        [Test]
        public void ThirtyDays()
        {
            StringBuilder fakeoutput = new StringBuilder();
            Console.SetOut(new StringWriter(fakeoutput));
            Console.SetIn(new StringReader("a\n"));

            Program.Main(new string[] { });
            String output = fakeoutput.ToString();
            Approvals.Verify(output);
        }

        [Test]
        public void ThirtyDays_StrategyPatternExample()
        {
            StringBuilder fakeoutput = new StringBuilder();
            Console.SetOut(new StringWriter(fakeoutput));
            Console.SetIn(new StringReader("a\n"));

            Program.Main(new string[] { typeof(GildedRoseStrategyPatternExample).Name });
            String output = fakeoutput.ToString();
            Approvals.Verify(output);
        }
    }
}
