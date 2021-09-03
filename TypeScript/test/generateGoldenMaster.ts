import * as fs from "fs";
import {generateReport, getItems, goldenMasterFilename} from "./generateReport";
import {GildedRose} from "../app/gilded-rose";


const report = generateReport(new GildedRose(getItems()))
fs.writeFileSync(goldenMasterFilename, report.join('\n'))