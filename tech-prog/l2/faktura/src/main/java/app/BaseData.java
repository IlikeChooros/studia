package app;

import java.util.Map;

interface InnerBaseData {
    /** Get the unique ID of the data object.
     * @return The unique ID.
     */
    int getId();

    /**
     * Return all field names with their display names.
     * @return Map of field names to display names
     */
    Map<String, String> getAllFieldNames();

    /**
     * Update a field with given values.
     * @param field The field name to update
     * @param values The values to set
     * @return true if update was successful, false otherwise
     */
    boolean updateField(String field, Object... values);

    /**
     * Get the value of a specific field.
     * @param fieldName The name of the field
     * @return The value of the field
     */
    Object getValue(String fieldName);
}

public abstract class BaseData implements InnerBaseData {

    /** ID counter for generating unique IDs. */
    private static int idCounter = 0;

    /** Unique ID for the data object. */
    protected int id;

    /** Constructor for DataLike class. */
    public BaseData() {
        this.id = idCounter++;
    }

    /** Get the unique ID of the data object.
     * @return The unique ID.
     */
    public int getId() {
        return id;
    }

    // Simply return all field names
    // public String[] GetAllFieldNames();

    // // May throw some kind of exception if invalid
    // public void UpdateField(String field, Object... values);
}
