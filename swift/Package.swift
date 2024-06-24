// swift-tools-version:5.5

import PackageDescription

let package = Package(
    name: "GildedRose",
    products: [
        .library(
            name: "GildedRose",
            targets: ["GildedRose"]
        ),
    ],
    targets: [
        .target(
            name: "GildedRose",
            dependencies: []
        ),
        .executableTarget(
            name: "GildedRoseApp",
            dependencies: ["GildedRose"]
        ),
        .testTarget(
            name: "GildedRoseTests",
            dependencies: ["GildedRose"]
        ),
    ]
)
