
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define UNDEFINED_STATE UINT32_MAX

#define PLACES 20
#define TRANSITIONS 10

struct term* places[PLACES];



////////////////////////////////////////////////////////////////////////////////
// Term Rewriting
////////////////////////////////////////////////////////////////////////////////

typedef enum { INT, DOUBLE, CHAR, STRING, FUNCTION, FUTURE } term_type;

struct term {
  term_type type;
  uint32_t state;
  uint8_t references;
  union {
    int int_value;
    double double_value;
    char char_value;
    char* string_value;
    struct function* function;
    int future;
  } value;
};

struct function {
  uint32_t symbol;
  uint32_t arity;
  struct term* arguments[];
};

struct term* rewrite(struct term*);

////////////////////////////////////////////////////////////////////////////////
// Garbage Collector
////////////////////////////////////////////////////////////////////////////////

int allocated_terms = 0;
struct term* allocated_term[65536];

bool deallocate_term(struct term* term) {
  if(term->references > 0) {
    return false;
  }
  
  switch(term->type) {
  case FUNCTION:
    for(int i = 0; i < term->value.function->arity; i++) {
      term->value.function->arguments[i]->references--;
      deallocate_term(term->value.function->arguments[i]);
    }
    free(term);
    return true;
    
  case STRING:
    free(term->value.string_value);
    free(term);
    return true;
    
  case INT:
  case DOUBLE:
  case CHAR:
  case FUTURE:
    free(term);
    return true;
  }
}

struct term* allocate_int(int value) {
  struct term* term = malloc(sizeof *term);
  term->type = INT;
  term->state = UNDEFINED_STATE;
  term->references = 1;
  term->value.int_value = value;
  allocated_term[allocated_terms++] = term;
  return term;
}

struct term* allocate_double(int value) {
  struct term* term = malloc(sizeof *term);
  term->type = DOUBLE;
  term->state = UNDEFINED_STATE;
  term->references = 1;
  term->value.double_value = value;
  allocated_term[allocated_terms++] = term;
  return term;
}

struct term* allocate_char(int value) {
  struct term* term = malloc(sizeof *term);
  term->type = CHAR;
  term->state = UNDEFINED_STATE;
  term->references = 1;
  term->value.char_value = value;
  allocated_term[allocated_terms++] = term;
  return term;
}

struct term* allocate_string(char* string) {
  struct term* term = malloc(sizeof *term);
  term->type = STRING;
  term->state = UNDEFINED_STATE;
  term->references = 1;
  term->value.string_value = calloc(1 + strlen(string), sizeof(char));
  strcpy(term->value.string_value, string);
  allocated_term[allocated_terms++] = term;
  return term;
}

struct term* allocate_function(uint32_t symbol, uint32_t arity, struct term* arguments[]) {
  struct function* function = malloc(arity * sizeof(struct term*) + sizeof *function);
  function->symbol = symbol;
  function->arity = arity;
  for(int i = 0; i < arity; i++) {
    function->arguments[i] = arguments[i];
  }
  
  struct term* term = malloc(sizeof *term);
  term->type = FUNCTION;
  term->state = UNDEFINED_STATE;
  term->references = 1;
  term->value.function = function;
  allocated_term[allocated_terms++] = term;
  return term;
}

struct term* allocate_future(uint32_t symbol, uint32_t arity, struct term* arguments[]) {
  
}

struct term* copy_term(struct term* term) {
  switch(term->type) { 
  case INT:
    return allocate_int(term->value.int_value);
  case DOUBLE:
    return allocate_double(term->value.double_value);
  case CHAR:
    return allocate_char(term->value.char_value);
  case STRING:
    return allocate_string(term->value.string_value);
  case FUNCTION: ;
    struct term** arguments = calloc(term->value.function->arity, sizeof *arguments);
    for(int i = 0; i < term->value.function->arity; i++) {
      arguments[i] = copy_term(term->value.function->arguments[i]);
    }
    return allocate_function(term->value.function->symbol, term->value.function->arity, arguments);
  }
}

void garbage_collect() {
  int i = 0;
  while(i < allocated_terms) {
    if(deallocate_term(allocated_term[i])) {
      allocated_term[i] = allocated_term[--allocated_terms];
    } else {
      i++;
    }
  }
}

void print_term(struct term* term) {
  switch(term->type) {
  case INT:
    printf("%d", term->value.int_value);
    return;
  case DOUBLE:
    printf("%f", term->value.double_value);
    return;
  case CHAR:
    printf("%c", term->value.char_value);
    return;
  case STRING:
    printf("%s", term->value.string_value);
    return;
  case FUNCTION:
    if(term->value.function->arity == 0) {
      printf("term_%d", term->value.function->symbol);
      return;
    }

    printf("term_%d(", term->value.function->symbol);
    for(int i = 0; i < term->value.function->arity; i++) {
      print_term(term->value.function->arguments[i]);
      if(i + 1 < term->value.function->arity) {
	printf(", ");
      }
    }
    printf(")");
    return;
  case FUTURE:
    printf("?%d", term->value.future);
    return;
  }
}

struct term* normalize(struct term* term) {
  if(term->type != FUNCTION) {
    return term;
  }

  struct term* previous_term = NULL;
  do {
    for(int i = term->value.function->arity-1; i >= 0; i--) {
      term->value.function->arguments[i] = normalize(term->value.function->arguments[i]);
    }
    previous_term = term;
    term = rewrite(term);
  } while(term != NULL);

  return previous_term;
}

////////////////////////////////////////////////////////////////////////////////
// Future/Promise handling
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// Petri Net Semantics
////////////////////////////////////////////////////////////////////////////////

uint32_t enabled = 0;
uint32_t dense[TRANSITIONS];
uint32_t sparse[TRANSITIONS];
void (*fire[TRANSITIONS])();



bool is_enabled(int transition) {
  if(dense[transition] >= enabled) {
    return false;
  }

  return transition == sparse[dense[transition]];
}

bool enable(int transition) {
  if(is_enabled(transition)) {
    return false;
  }

  dense[transition] = enabled;
  sparse[enabled] = transition;
  enabled++;
  
  return true;
}
 
bool disable(int transition) {
  if(!is_enabled(transition)) {
    return false;
  }

  enabled--;
  sparse[dense[transition]] = dense[sparse[enabled]];
  dense[sparse[enabled]] = dense[transition];
  
  return true;
}

uint32_t choose() {
  if(enabled == 0) {
    return UINT32_MAX;
  }

  return sparse[0];
}

////////////////////////////////////////////////////////////////////////////////
// Generated Code
////////////////////////////////////////////////////////////////////////////////

#define TERM_f 0
#define TERM_zero 1
#define TERM_succ 2
#define TERM_even 3
#define TERM_odd 4

struct term* rewrite(struct term* term) {
  if(term->type != FUNCTION) {
    return NULL;
  } else if(term->value.function->symbol != TERM_f) {
    return NULL;
  } else if(term->value.function->arguments[0]->type != FUNCTION) {
    return NULL;
  } else if(term->value.function->arguments[0]->value.function->symbol == TERM_zero) {
    struct term* new_term = allocate_function(TERM_even, 0, NULL);
    return new_term;
  } else if(term->value.function->arguments[0]->value.function->symbol != TERM_succ) {
    return NULL;
  } else if(term->value.function->arguments[0]->value.function->arguments[0]->type != FUNCTION) {
    return NULL;
  } else if(term->value.function->arguments[0]->value.function->arguments[0]->value.function->symbol == TERM_zero) {
    struct term* new_term = allocate_function(TERM_odd, 0, NULL);
    return new_term;
  }
  
  struct term* arguments[] = { term->value.function->arguments[0]->value.function->arguments[0]->value.function->arguments[0] }; 
  struct term* new_term = allocate_function(TERM_f, 1, arguments);
  return new_term;
}


int main() {
  struct term* z = allocate_function(TERM_zero, 0, NULL);
  
  struct term* arguments_s[1] = { z };
  struct term* sz = allocate_function(TERM_succ, 1, arguments_s);

  struct term* arguments_ss[1] = { sz };
  struct term* ssz = allocate_function(TERM_succ, 1, arguments_ss);

  struct term* arguments_sss[1] = { ssz };
  struct term* sssz = allocate_function(TERM_succ, 1, arguments_sss);

  struct term* arguments_ssss[1] = { sssz };
  struct term* ssssz = allocate_function(TERM_succ, 1, arguments_ssss);

  struct term* arguments_sssss[1] = { ssssz };
  struct term* sssssz = allocate_function(TERM_succ, 1, arguments_sssss);

  struct term* arguments_fsssss[1] = { sssssz };
  struct term* fsssssz = allocate_function(TERM_f, 1, arguments_fsssss);

  print_term(fsssssz);
  printf("\n");
  print_term(normalize(fsssssz));
  printf("\n");
  
  printf("Allocated terms: %d\n", allocated_terms);
  garbage_collect();
  printf("Allocated terms: %d\n", allocated_terms);
  
}
