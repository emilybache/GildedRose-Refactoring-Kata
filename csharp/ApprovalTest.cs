using System;
using System.IO;
using System.Text;
using NUnit.Framework;

namespace csharp
{
    [TestFixture]
    public class ApprovalTest
    {
        [Test]
        public void ThirtyDays()
        {
            var lines = File.ReadAllLines("ThirtyDays.txt");

            StringBuilder fakeoutput = new StringBuilder();
            Console.SetOut(new StringWriter(fakeoutput));
            Console.SetIn(new StringReader("a\n"));

            Program.Main(new string[] { });
            String output = fakeoutput.ToString();

            var outputLines = output.Split('\n');
            for(var i = 0; i<Math.Min(lines.Length, outputLines.Length); i++) 
            {
                Assert.AreEqual(lines[i], outputLines[i]);
            }
        }
    }
}
