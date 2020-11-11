var { expect } = require("chai");
var { compose } = require("./texttest_fixture");
var fs = require("fs");
var path = require("path");
var expectedOutput = fs
  .readFileSync(path.join(__dirname, "approved.txt"))
  .toString();

describe("Gilded Rose", function () {
  it("should match", function () {
    const output = compose(31);
    expect(output).to.equal(expectedOutput);
  });
});
