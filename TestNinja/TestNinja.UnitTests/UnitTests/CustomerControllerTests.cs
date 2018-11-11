using System.Security.Cryptography.X509Certificates;
using NUnit.Framework;
using TestNinja.Fundamentals;

namespace TestNinja.UnitTests.UnitTests
{
    [TestFixture]
    class CustomerControllerTests
    {
        private CustomerController _customerController;

        [SetUp]
        public void SetUp()
        {
            _customerController = new CustomerController();
        }

        [Test]
        public void GetCustomer_IfIdIsGreaterThenZero_ReturnOk()
        {
            var result = _customerController.GetCustomer(4);
            Assert.That(result, Is.InstanceOf<ActionResult>());
            Assert.That(result, Is.TypeOf<Ok>());

        }
    }
}
