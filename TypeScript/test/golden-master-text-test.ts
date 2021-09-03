import {generateReport, getItems} from "./generateReport";
import {GildedRose} from "../app/gilded-rose";

const outputLines = generateReport(new GildedRose(getItems()));
console.log(outputLines.join('\n'))