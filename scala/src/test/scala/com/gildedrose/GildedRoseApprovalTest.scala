package com.gildedrose

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{File, FileOutputStream}
import scala.io.Source

class GildedRoseApprovalTest extends AnyFlatSpec with Matchers {
  it should "return the result of the golden master" in {
    val fileWithTestResult = new File("approvaltests/gildedrose.testresult.txt")
    val outputStream       = new FileOutputStream(fileWithTestResult)
    Console.withOut(outputStream) {

      TexttestFixture.main(Array("30"))
    }

    val approvedFile          = "approvaltests/gildedrose.approved.txt"
    val sourceForTestResults  = Source.fromFile(fileWithTestResult)
    val sourceForApprovedFile = Source.fromFile(approvedFile)

    val resultingOutput =
      try sourceForTestResults.getLines().toVector
      finally sourceForTestResults.close()
    val approvedOutput =
      try sourceForApprovedFile.getLines().toVector
      finally sourceForTestResults.close()

    resultingOutput should equal(approvedOutput)
  }

}
