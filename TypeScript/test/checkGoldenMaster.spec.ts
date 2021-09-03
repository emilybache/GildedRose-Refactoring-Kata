import * as fs from "fs";
import {generateReport, getItems, goldenMasterFilename} from "./generateReport";
import {GildedRose} from "../app/gilded-rose";
import { expect } from "chai";

function getGoldenMaster() {
    try {
        return fs.readFileSync(goldenMasterFilename).toString().split('\n')
    }catch {
        throw new Error('No golden master file generated; run `yarn create-master` or `npm run create-master`')
    }
}

describe('gilded rose golden master', () => {
    it('should match golden master', () => {
        const goldenMaster = getGoldenMaster();

        const newReport = generateReport(new GildedRose(getItems()))

        expect(newReport.length).to.equal(goldenMaster.length)

        goldenMaster.forEach((line, index) => {
            expect(newReport[index]).to.equal(line)
        })
    })
})