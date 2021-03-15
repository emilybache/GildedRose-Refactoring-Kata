# Gilded Rose Requirements Specification

Hi and welcome to team Gilded Rose. As you know, we are a small inn with a 
prime location in a prominent city ran by a friendly innkeeper named 
Allison. We also buy and sell only the finest goods. Unfortunately, our 
goods are constantly degrading in quality as they approach their sell by 
date. We have a system in place that updates our inventory for us. It was 
developed by a no-nonsense type named Leeroy, who has moved on to new 
adventures. Your task is to add the new feature to our system so that we 
can begin selling a new category of items. 

First an introduction to our system:

- สินค้าแต่ละชิ้นจะต้องขายภายในระยะเวลาที่กำหนดไว้ในตัวแปร "SellIn" (มีหน่วยเป็น*วัน*) 
- สินค้าแต่ละชิ้นมีตัวแปร "Quality" ที่สามารถบ่งชี้ถึงมูลค่าของสินค้า
- ระบบจะทำการคำนวนค่า "SellIn" และ "Quality" ของสินค้าทุกชิ้นในช่วงเวลาสุดท้ายของทุกวัน

ง่ายอะดิ, ใช่ป่ะ? Well this is where it gets interesting:

- เมื่อสินค้าเลยกำหนดขายไปแล้ว "Quality" ของสินค้าจะลดลงเป็นสองเท่า
- "Quality" ของสินค้าไม่มีทางติดลบได้
- คุณภาพของสินค้าประเภท "Aged Brie" จะเพิ่มสูงขึ้นตามระยะเวลา
- "Quality" มีค่าสูงสุดคือ 50 
- สินค้าประเภท "Sulfuras" เป็นสินค้าในตำนานหายาก คุณภาพของสินค้าจะไม่ลดลงและไม่ได้มีไว้เพื่อขาย
- สินค้าประเภท "Backstage passes" คุณภาพของสินค้าเหมือนกันกับสินค้าประเภท aged brie 
เพียงแต่หากใกล้ถึงวันแสดง 10 วันก่อนหน้าหรือน้อยกว่าคุณภาพของสินค้าประเภทนี้จะเพิ่มทีละ 2  
และถ้าใกล้ถึงวันแสดง 5 วันหรือน้อยกว่าคุณภาพจะเพิ่มทีละ 3
อย่างไรก็ตามคุณภาพของสินค้าจะกลายเป็น 0 ทันทีหลังการแสดงจบลง

เมื่อเร็วๆ นี้เราพึ่งได้ลงนามกับผู้ผลิตสินค้าประเภทของขลัง และเราต้องการที่จะเพิ่มความสามารถใหม่เข้าไปในระบบ:

- คุณภาพสินค้าประเภท "Conjured" จะลดคุณภาพลงเร็วกว่าสินค้าปกติถึงสองเท่า

Feel free to make any changes to the UpdateQuality method and add any 
new code as long as everything still works correctly. However, do not 
alter the Item class or Items property as those belong to the goblin 
in the corner who will insta-rage and one-shot you as he doesn't 
believe in shared code ownership (you can make the UpdateQuality 
method and Items property static if you like, we'll cover for you).

Just for clarification, an item can never have its Quality increase 
above 50, however "Sulfuras" is a legendary item and as such its 
Quality is 80 and it never alters.
