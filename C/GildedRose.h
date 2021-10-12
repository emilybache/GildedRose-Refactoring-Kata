#ifndef ROSE_INCLUDED
#define ROSE_INCLUDED

typedef struct
{
    char *name;
    int sellIn;
    int quality;
} Item;

extern Item* init_item(Item* item, const char *name, int sellIn, int quality);
extern void update_quality(Item items[], int size);
extern char* print_item(char* buffer, Item* item);

#endif