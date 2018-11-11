using System.Linq;
using System.Runtime.ExceptionServices;
using NUnit.Framework;
using TestNinja.Fundamentals;

namespace TestNinja.UnitTests.UnitTests
{
    [TestFixture]
    class MathTests
    {
        private Math _math;

        // SetUp - before tests
        [SetUp]
        public void SetUp()
        {
            _math = new Math();
        }
        // TearDown - after tests


        [Test]
        [TestCase(1,2,3)]
        [TestCase(0, 0, 0)]
        [TestCase(10,20,30)]
        public void Add_WhenCalled_ReturnTheSUmOfArguments(int first, int second, int expectedResult)
        {
            var result = _math.Add(first, second);

            Assert.That(result, Is.EqualTo(expectedResult));
        }

        [Test]
        [Ignore("Because I wanted to!")]
        public void Max_FirstArgumentIsGreater_ReturnTheFirstArgument()
        {
            var result = _math.Max(5, 2);
            Assert.That(result, Is.EqualTo(5));
        }

        [Test]
        public void Max_SecondArgumentIsGreater_ReturnTheSecondArgument()
        {
            var result = _math.Max(5, 15);
            Assert.That(result, Is.EqualTo(15));
        }

        [Test]
        public void Max_ArgumentsAreEqual_ReturnTheSameArgumentArgument()
        {
            var result = _math.Max(10, 10);
            Assert.That(result, Is.EqualTo(10));
        }

        [Test]
        public void GetOddNumbers_limitIsGreaterThanZero_ReturnOddNumbersUpToLimit()
        {
            var result = _math.GetOddNumbers(5);

            Assert.That(result, Is.Not.Empty);
            Assert.That(result.Count(), Is.EqualTo(3));
            Assert.That(result, Does.Contain(1));
            Assert.That(result, Does.Contain(3));
            Assert.That(result, Does.Contain(5));

            Assert.That(result, Is.EquivalentTo(new [] {1, 3, 5}));

            Assert.That(result, Is.Ordered);
            Assert.That(result, Is.Unique);


        }
    }
}
