// key_listener.h
// The public header file for our C library. This defines the minimal API
// that the Haskell application will interact with.

#ifndef KEY_LISTENER_H
#define KEY_LISTENER_H

#include <stdbool.h>

// Define a function pointer type for the callback.
// This is the function that our C code will call back into Haskell with.
// event_type: 10 for KeyDown, 11 for KeyUp
// key_code: The raw virtual keycode.
// RETURN: 1 to swallow the event, 0 to pass it through.
typedef int (*haskell_key_callback)(int event_type, int key_code);

// Starts the key listener on a background thread.
// Takes a pointer to the Haskell callback function.
// Returns 0 on success, -1 on failure.
int start_key_listener(haskell_key_callback callback);

// Stops the key listener and cleans up the background thread.
void stop_key_listener(void);

// Synthesizes a new keyboard event and posts it to the system.
void send_key_event(int key_code, bool is_down);

#endif // KEY_LISTENER_H
