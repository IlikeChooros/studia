package app;

import java.util.Vector;

public interface ManagerLike<T extends DataLike> {
    /** Adds an item to the collection.
     * @param item Item to add.
     */
    void add(T item);

    /** Removes an item from the collection.
     * @param item Item to remove.
     */
    void remove(T item);

    /** Searches for items matching the query.
     * @param query The search query.
     * @return A vector of matching items.
     */
    Vector<T> search(String query);

    /** Gets an item by its ID.
     * @param id The ID of the item.
     * @return The item with the specified ID, or null if not found.
     */
    T getById(int id);

    /** Returns all items in the collection.
     * @return The collection of items.
     */
    Vector<T> getAll();
}
