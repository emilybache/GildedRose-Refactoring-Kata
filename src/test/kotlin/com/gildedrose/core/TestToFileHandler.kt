package com.gildedrose.core

import java.io.File

class TestToFileHandler(
        fileName: String) {

    val filePath = "texttest/$fileName.txt"

    fun write(text: String) = File(filePath).bufferedWriter().use { it.write(text) }

    fun read() = File(filePath).bufferedReader().readLines().joinToString(separator = "\n")
}