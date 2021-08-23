// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "GildedRose",
    products: [
        .library(
            name: "GildedRose",
            targets: ["GildedRose"]),
    ],
    targets: [
        .target(
            name: "GildedRose",
            dependencies: []),
        .target(
            name: "GildedRoseApp",
            dependencies: ["GildedRose"]),
        .testTarget(
            name: "GildedRoseTests",
            dependencies: ["GildedRose"]),
    ]
)
