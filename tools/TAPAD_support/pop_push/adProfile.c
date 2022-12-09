#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <time.h>
#include <unistd.h>

// Access to the cycle counter
#define rdtscll(val) \
     __asm__ __volatile__ ("rdtsc" : "=A" (val))

#define ENABLE_LINE_RECORDING 1

unsigned long long int mytime_() {
        unsigned long long int time;

        rdtscll(time);
        return time ;
}

// Access to stacks size
extern long int bigStackSize;
extern long int smallstacksize_();


// Declaration of the event list data structures
#define ARRAY_SIZE 100000
struct event {
                int kind;
                char *function;
                int len;
                unsigned long long int time;
                int stacksize;
#ifdef ENABLE_LINE_RECORDING
                int line;
#endif
};

struct list_node {
        struct event array[ARRAY_SIZE];
        struct list_node *next;
};
static struct list_node hdcell =  { .next = 0 };
static struct list_node *tlcell = &hdcell;
static int cell_index = 0;

// Function to traverse the list of events
static void traverse_event_list(void (*process)(struct event *ev, void *data), void *data) {
        struct list_node *c = &hdcell;
        
        while (c) {
                int max = c->next ? ARRAY_SIZE : cell_index;
                int i;
                for (i = 0; i < max; ++i)
                        process(&c->array[i], data);
                c = c->next;
        }
}


// Declaration of profiling functions
#if 1
#define profiledebug 
#else
#define profiledebug(type) \
        printf("%50s %02u %022llu %010lu %li\n", buffer, type, mytime_(), bigStackSize, smallstacksize_());
#endif
enum { BEGIN = 1, END, ENDFWD, BEGINSNAPSHOT, ENDSNAPSHOT, ENDORIG };

int init = 0;
unsigned long long int beginning_of_time = 0;
inline static void profile_real(char *function, int flen, int kind) {
	static struct list_node *new = 0;
        if (cell_index >= ARRAY_SIZE) {
                new = (struct list_node*)calloc(1, sizeof(struct list_node));
                tlcell->next = new;
                tlcell = new;
                new->next = 0;
                cell_index = 0;
        }
        static unsigned long long int time;
	time = mytime_();
        if (init == 0)
          beginning_of_time = time, init = 1;
        tlcell->array[cell_index].kind = kind;
        tlcell->array[cell_index].function = function;
        tlcell->array[cell_index].len = flen;
        tlcell->array[cell_index].time = time - beginning_of_time;
        tlcell->array[cell_index].stacksize = bigStackSize + smallstacksize_();
        #if 0
        printf("%016llu %09lu %02hhu ", time-beginning_of_time, bigStackSize + smallstacksize_(), kind);
        fwrite(function, 1, flen, stdout);
        printf("\n");
        #endif
        cell_index++;
}

#define declare_profile(suffix,type) \
void profile##suffix##_(char *function, int flen) {\
  profile_real(function, flen, type);\
}

declare_profile(begin,BEGIN)
declare_profile(end,END)
declare_profile(endfwd,ENDFWD)
declare_profile(beginsnapshot,BEGINSNAPSHOT)
declare_profile(endsnapshot,ENDSNAPSHOT)
declare_profile(endorig,ENDORIG)

#ifdef ENABLE_LINE_RECORDING
static int current_line_number = 0;

void profileline_(int *line) {
    current_line_number = *line;
}
#endif

// Hashtable data structure
#define hashtbl_size 4093
struct hash_cell {
  char *function;
  int len;
} hashtbl[hashtbl_size];
// Count of the used element in the hashtable
int hashtbl_count;

// Simple hash function
static inline unsigned int hash(unsigned int u) {
  return ((u % hashtbl_size) * 1024 ) % hashtbl_size;
}

// Simple hash function on strings
static inline unsigned int hash_string(char *string, int flen) {
  unsigned int ret = 0x55555555;
  while (flen--)
    ret *= ((unsigned char)*string++ * hashtbl_size);
  return hash(ret);
}

// Min macro
#define min(a,b) (a < b ? a : b)

// Get the id of a string address, allocate a bucket if new
static int get(char *function, int flen) {
  int h = hash_string(function, flen);
  while (hashtbl[h].function) {
         if (strncmp(hashtbl[h].function, function, min(flen, hashtbl[h].len)) == 0)
            break;
         h = hash(h);
  }
  hashtbl[h].function = function;
  hashtbl[h].len = flen;
  return h;
} 

// Functions to serialize integers and known length strings
static inline void fwrite_int(FILE *f, int i) {
  fwrite(&i, sizeof(int), 1, f);
}
static inline void fwrite_llint(FILE *f, unsigned long long int i) {
  fwrite(&i, sizeof(unsigned long long int), 1, f);
}
static inline void fwrite_string(FILE *f, char *str, int len) {
  fwrite_int(f, len);
  fwrite(str, (unsigned int) len, 1, f);
}

// Function to build the hashtable of function names
static void add_event_to_hashtable(struct event *ev, void *data);
static void build_hashtable() {
        
        // Initialize the hashtable
        memset(hashtbl, 0, sizeof(struct hash_cell)*hashtbl_size);
        /** Fill the hashtbl */
        traverse_event_list(add_event_to_hashtable, 0);
        /** Compute the size of the hashtbl */
        hashtbl_count = 0;
        int i;
        for (i=0; i < hashtbl_size; ++i)
          if (hashtbl[i].function)
            hashtbl_count++;
}
static void add_event_to_hashtable(struct event *ev, void *data) {
  int h = get(ev->function, ev->len);
}


// Serialize event list into a file named tapenade.prof
struct print_args {
        FILE *prof;
        struct event **stack;
        int *stack_counter;
};
static void print_an_event(struct event *ev, void *data);
void printprofile_() {
        int i = 0;
        FILE *prof = fopen("tapenade.prof", "w");
        if (!prof) {
                perror("Can't open tapenade.prof");
                exit(1);
        }
        // Compress the output;
        // Generate a hashtable of the functions name
        // use it to make a map from function name 
        // to numeric id
        build_hashtable();

        // Position at the beginning
        fseek(prof, 0, SEEK_SET);
        
        // Dump the hash table
        fwrite_int(prof, hashtbl_count);
        for (i=0; i < hashtbl_size; ++i)
          if (hashtbl[i].function) {
            fwrite_int(prof, i);
            char *string = hashtbl[i].function;
            int string_length = hashtbl[i].len;
            fwrite_string(prof, string, string_length);
          }
          
        // Dump the events
        struct list_node *c = &hdcell;
        struct event *stack[1000];
        int stack_counter = -1;
        struct print_args args = { 
                .prof = prof, 
                .stack = stack, 
                .stack_counter = &stack_counter 
        };
        traverse_event_list(print_an_event, &args);
        
        // For program that aborts too abruptly ( with STOP )
        // Use the return stack image to recreate EXIT events
        for (i=stack_counter; i>-1;i--) {
          struct event *cur = stack[i];
          int h = get(cur->function, cur->len);
          fwrite_int(prof, h);
          fwrite_int(prof, 2); // Explicit ending
          fwrite_llint(prof, cur->time);
          fwrite_int(prof, cur->stacksize);
        }
        fclose(prof);
}
void halt_();
// Utility function for printprofile
static void print_an_event(struct event *ev, void *data) {
        struct print_args *args = (struct print_args*)data;
        static unsigned long long int last_time = 0;
        
        int h = get(ev->function, ev->len);
        // Keep trace of the return stack depth and values
        switch (ev->kind) {
          case 1:
            *(args->stack_counter) += 1;
            args->stack[*(args->stack_counter)] = ev;
            break;
          case 2:
            *(args->stack_counter) -= 1;
            break;
        }
//DEBUG        printf("Time: %llu\n", ev->time - begin_time);
        if (last_time && ev->time <= last_time) {
                printf("Erreur time <= last_time: %llu <= %llu\n", time, last_time);
                halt_();
        }
        fwrite_int(args->prof, h);
        fwrite_int(args->prof, ev->kind);
        fwrite_llint(args->prof, ev->time);
        fwrite_int(args->prof, ev->stacksize);
        last_time = ev->time;
}

#include <signal.h>
void halt_() {
        kill(getpid(), SIGTRAP);
}
