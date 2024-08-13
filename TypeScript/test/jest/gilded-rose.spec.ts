import { Item, GildedRose } from '@/gilded-rose';

describe('Gilded Rose', () => {
  it('should foo', () => {
    const gildedRose = new GildedRose([new Item('foo', 0, 0)]);
    const execTimes: number[] = [];
    for (let i = 0; i < 100; i++) {
      const startTime = performance.now();
      const items = gildedRose.updateQuality();
      const endTime = performance.now();

      const execTime = endTime - startTime;
      execTimes.push(execTime);

      console.log('Execution time: ' + (execTime) + 'ms');
      expect(items[0].name).toBe('foo');
    }

    const avgExecTime = execTimes.reduce((acc, curr) => acc + curr, 0) / execTimes.length;
    console.log('Average execution time: ' + avgExecTime + 'ms');
  });
});

