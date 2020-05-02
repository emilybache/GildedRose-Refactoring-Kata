<h3>How to Run Tests</h3>
<ul>
    <li>CLI - gradlew :cleanTest :test --tests "com.gildedrose.GildedRoseTest.foo"</li>
    <li>Intellij
        <ul>
            <li>Tasks: :cleanTest :test<br/></li>
            <li>Arguments: --tests "com.gildedrose.GildedRoseTest.foo"</li>
        </ul>
    </li>
</ul>

<h3>The System</h3>
<ul>
    <li>
        All items have a SellIn value which denotes the number of days we 
        have to sell the item.
    </li>
    <li>All items have a Quality value which denotes how valuable the item is.</li>
    <li>At the end of each day our system lowers both values for every item.</li>
    <li>Once the sell by date has passed, Quality degrades twice as fast</li>
    <li>The Quality of an item is never negative</li>
    <li>"Aged Brie" actually increases in Quality the older it gets</li>
    <li>The Quality of an item is never more than 50</li>
    <li>"Sulfuras", being a legendary item, never has to be sold or decreases in Quality</li>
    <li>
    "Backstage passes", like aged brie, increases in Quality as its SellIn value approaches;
        <ul>
            <li>Quality increases by 2 when there are 10 days or less and by 3 when there are 5 days or less but</li>
            <li>Quality drops to 0 after the concert</li>
        </ul>
    </li>
</ul>

<h3>New Requirement</h3>
<ul>
    <li>Conjured" items degrade in Quality twice as fast as normal items</li>
</ul>

<h3>Restrictions</h3>
<ul>
    <li>Do not alter the Item class or Items property</li>
    <li>
        An item can never have its Quality increase above 50, however "Sulfuras" is a
        legendary item and as such its Quality is 80 and it never alters.
    </li>
</ul>

<h3>Original Output</h3>
<div>OMGHAI!</div>
<div>-------- day 0 --------</div>
<table>
    <thead>
        <tr>
            <th>name</th>
            <th>sellIn</th>
            <th>quality</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>+5 Dexterity Vest</td>
            <td>10</td>
            <td>20</td>
        </tr>
        <tr>
            <td>Aged Brie</td>
            <td>2</td>
            <td>0</td>
        </tr>
        <tr>
            <td>Elixir of the Mongoose</td>
            <td>5</td>
            <td>7</td>
        </tr>
        <tr>
            <td>Sulfuras, Hand of Ragnaros</td>
            <td>0</td>
            <td>80</td>
        </tr>
        <tr>
            <td>Sulfuras, Hand of Ragnaros</td>
            <td>-1</td>
            <td>80</td>
        </tr>
        <tr>
            <td>Backstage passes to a TAFKAL80ETC concert</td>
            <td>15</td>
            <td>20</td>
        </tr>
        <tr>
            <td>Backstage passes to a TAFKAL80ETC concert</td>
            <td>10</td>
            <td>49</td>
        </tr>
        <tr>
            <td>Backstage passes to a TAFKAL80ETC concert</td>
            <td>5</td>
            <td>49</td>
        </tr>
        <tr>
            <td>Conjured Mana Cake</td>
            <td>3</td>
            <td>6</td>
        </tr>
    </tbody>
</table>
<div>-------- day 1 --------</div>
<table>
    <thead>
        <tr>
            <th>name</th>
            <th>sellIn</th>
            <th>quality</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>+5 Dexterity Vest</td>
            <td>9</td>
            <td>19</td>
        </tr>
        <tr>
            <td>Aged Brie</td>
            <td>1</td>
            <td>1</td>
        </tr>
        <tr>
            <td>Elixir of the Mongoose</td>
            <td>4</td>
            <td>6</td>
        </tr>
        <tr>
            <td>Sulfuras, Hand of Ragnaros</td>
            <td>0</td>
            <td>80</td>
        </tr>
        <tr>
            <td>Sulfuras, Hand of Ragnaros</td>
            <td>-1</td>
            <td>80</td>
        </tr>
        <tr>
            <td>Backstage passes to a TAFKAL80ETC concert</td>
            <td>14</td>
            <td>21</td>
        </tr>
        <tr>
            <td>Backstage passes to a TAFKAL80ETC concert</td>
            <td>9</td>
            <td>50</td>
        </tr>
        <tr>
            <td>Backstage passes to a TAFKAL80ETC concert</td>
            <td>4</td>
            <td>50</td>
        </tr>
        <tr>
            <td>Conjured Mana Cake</td>
            <td>2</td>
            <td>5</td>
        </tr>
    </tbody>
</table>