package app;

import java.util.Map;

interface DataLike {
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
