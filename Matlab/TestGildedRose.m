classdef TestGildedRose < matlab.unittest.TestCase

    methods(Test)
        % Test methods
        function test_standard_item(tc)
            items = [Item("foo", 4, 5)];
            gilded_rose = GildedRose(items);
            gilded_rose.update_quality();
            tc.verifyEqual(items(1).name, "Fixme")
        end
    end
end