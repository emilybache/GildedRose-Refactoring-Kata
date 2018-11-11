using System;
using NUnit.Framework;
using NUnit.Framework.Internal;
using TestNinja.Fundamentals;

namespace TestNinja.UnitTests.UnitTests
{
    [TestFixture]
    class StackTests
    {
        [Test]
        public void Push_PassNull_ThrowArgumentNullException()
        {
            var stack = new Stack<object>();
            Assert.That(() => stack.Push(null), Throws.ArgumentNullException);
        }

        [Test]
        public void Push_PassObject_StackIsNotEmpty()
        {
            var stack = new Stack<object>();
            stack.Push(new object());
            Assert.That(stack.Count, Is.GreaterThan(0));
        }

        [Test]
        public void Count_EmptyStack_ReturnZero()
        {
            var stack = new Stack<object>();
            Assert.That(stack.Count, Is.EqualTo(0));
        }

        [Test]
        public void Pop_EmptyStack_ThrowInvalidOperationException()
        {
            var stack = new Stack<object>();
            Assert.That(() => stack.Pop(), Throws.InvalidOperationException);
        }

        [Test]
        public void Pop_StackNotEmpty_ReturnTopElement()
        {
            var stack = new Stack<string>();
            stack.Push("dog");
            stack.Push("mouse");
            stack.Push("cat");

            var result = stack.Pop();

            Assert.That(result, Is.EqualTo("cat"));
        }

        [Test]
        public void Pop_StackNotEmpty_RemoveTopElement()
        {
            var stack = new Stack<string>();
            stack.Push("dog");
            stack.Push("mouse");
            stack.Push("cat");

            stack.Pop();
            var result = stack.Peek();

            Assert.That(stack.Count, Is.EqualTo(2));
        }

        [Test]
        public void Peek_EmptyStack_ThrowInvalidOperationException()
        {
            var stack = new Stack<string>();
            Assert.That(() => stack.Peek(), Throws.InvalidOperationException);
        }

        [Test]
        public void Peek_NotEmptyStack_ReturnElementAtTheTop()
        {
            var stack = new Stack<string>();

            stack.Push("dog");
            var result = stack.Peek();

            Assert.That(result, Is.EqualTo("dog"));
        }
    }
}
