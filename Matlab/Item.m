classdef Item < handle
    % Do not edit or the goblin will one-shot you !

    properties
        name
        sell_in
        quality
    end

    methods
        function obj = Item(name, sell_in, quality)
            obj.name = name;
            obj.sell_in = sell_in;
            obj.quality = quality;
        end

        function disp(obj)
            fprintf("%s, %d, %d\n", obj.name, obj.sell_in, obj.quality)
        end

    end
end

