# Tennis Court Registration

This app supports Admin Mode and User Mode using the URL.

------------------------------------------------------------------------

## How to Open as ADMIN

Admin mode is activated by adding ?admin=1 at the end of the app URL.

Example:

    http://127.0.0.1:xxxx/?admin=1

When in admin mode, you can: - Add courts
- Delete courts
- Set the price
- Manage bookings

------------------------------------------------------------------------

## How to Open as USER

Users open the normal URL without any admin flag:

    http://127.0.0.1:xxxx/

Users can: - Select a court (chosen by admin) - Choose a time slot -
Upload payment screenshot - Submit booking

Users cannot modify courts or price.

------------------------------------------------------------------------

## Changing Admin or User Mode

To access ADMIN mode:

Add this to the end of the URL:

    ?admin=1

To access USER mode:

Simply remove the admin flag:

    (no ?admin=1)

------------------------------------------------------------------------

## Default Courts

The app starts with the following courts:

-   Court 1
-   Court 2
-   Court 3
-   Court 4

Admin can add or remove courts at any time in admin mode.

------------------------------------------------------------------------

## File Saving

Court list is saved automatically inside:

    courts.rds

Booking data is saved inside:

    bookings.rds

------------------------------------------------------------------------

## Notes

If the court list becomes empty by mistake, admin can simply add new
courts again.

If you need to lock admin mode behind a password instead of URL access,
that can also be added upon request.
