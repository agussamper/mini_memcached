#include "../src/evict.h"
#include "../src/list.h"
#include "../src/cache.h"

#include <stdio.h>
#include <string.h>

Cache cache;

unsigned str_KRHash(const char *s) {
  unsigned hashval;
  for (hashval = 0; *s != '\0'; ++s) {
    hashval = *s + 31 * hashval;
  }
  return hashval;
}

void test1() {
  Evict evict;
  evict_init(&evict);
  List list[5]; 
  char kv[100]; 
  for(int i = 0; i < 5; i++) {
    list[i] = list_create();
    sprintf(kv, "%d", i);
    unsigned lenkv = strlen(kv);
    list_add(list+i, kv, lenkv, kv, lenkv, NULL);
    evict_add(evict, list[i], i, NULL);
  }
  evict_update(evict, list[4]);
  for(int i = 0; i < 5; i++) {
    evict_update(evict, list[i]);
  }
  evict_update(evict, list[2]);
  evict_update(evict, list[1]);

  NodeEvict node = evict_getLru(evict);
  NodeEvict node_n = evict_getNextNode(node);
  evict_removeNode(evict, node_n);
  list_remove_node(list+3, evict_getList(node_n));
  evict_removeNode(evict, node);
  list_remove_node(list+0, evict_getList(node));

  node = evict_getLru(evict);
  evict_removeNode(evict, node);
  list_remove_node(list+4, evict_getList(node));

  node = evict_getLru(evict);
  evict_removeNode(evict, node);
  list_remove_node(list+2, evict_getList(node));

  node = evict_getLru(evict);
  evict_removeNode(evict, node);
  list_remove_node(list+1, evict_getList(node));
}

void test2() {
  Evict evict;
  evict_init(&evict);
  List list[5]; 
  char kv[100]; 
  for(int i = 0; i < 5; i++) {
    list_create(list+i);
    sprintf(kv, "%d", i);
    unsigned lenkv = strlen(kv);
    list_add(list+i, kv, lenkv, kv, lenkv, NULL);
    evict_add(evict, list[i], i, NULL);
  }
  evict_remove(evict, list[0]);
  list_remove_node(list+0, list[0]);
  evict_remove(evict, list[4]);
  list_remove_node(list+4, list[4]);
  evict_remove(evict, list[2]);
  list_remove_node(list+2, list[2]);
  evict_remove(evict, list[1]);
  list_remove_node(list+1, list[1]);
  evict_remove(evict, list[3]);
  list_remove_node(list+3, list[3]);
}

int main() {
  cache = cache_create(1000, str_KRHash);
  test1();
  test2();
  return 0;
}