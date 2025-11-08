package app;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class ValidatorTest {

    @Test
    void testDateValidation() {
        // Valid format YYYY-MM-DD (regex-only, not semantic)
        assertTrue(Validator.isValidDate("2025-11-09"));
        // Invalid formats
        assertFalse(Validator.isValidDate("2025/11/09"));
        assertFalse(Validator.isValidDate("2025-1-9"));
        assertFalse(Validator.isValidDate("99-11-09"));
        assertFalse(Validator.isValidDate(null));
    }

    @Test
    void testIdentityNumbers() {
        // NIP: 10 digits
        assertTrue(Validator.isValidNIP("1234567890"));
        assertFalse(Validator.isValidNIP("123456789"));
        assertFalse(Validator.isValidNIP("12345678901"));
        assertFalse(Validator.isValidNIP("ABCDEFGHIJ"));
        assertFalse(Validator.isValidNIP(null));

        // ID number: 9 digits
        assertTrue(Validator.isValidIDNumber("123456789"));
        assertFalse(Validator.isValidIDNumber("12345678"));
        assertFalse(Validator.isValidIDNumber("1234567890"));
        assertFalse(Validator.isValidIDNumber("ABCDEF123"));
        assertFalse(Validator.isValidIDNumber(null));

        // PESEL: 11 digits
        assertTrue(Validator.isValidPESEL("12345678901"));
        assertFalse(Validator.isValidPESEL("1234567890"));
        assertFalse(Validator.isValidPESEL("123456789012"));
        assertFalse(Validator.isValidPESEL("ABCDEFGHIJK"));
        assertFalse(Validator.isValidPESEL(null));
    }

    @Test
    void testNumericValidators() {
        // Floats
        assertTrue(Validator.isValidFloat("0"));
        assertTrue(Validator.isValidFloat("0.0"));
        assertTrue(Validator.isValidFloat("12.34"));
        assertFalse(Validator.isValidFloat("-1"));
        assertFalse(Validator.isValidFloat("abc"));
        assertFalse(Validator.isValidFloat(null));

        // Ints
        assertTrue(Validator.isValidInt("0"));
        assertTrue(Validator.isValidInt("123"));
        assertFalse(Validator.isValidInt("-5"));
        assertFalse(Validator.isValidInt("12.3"));
        assertFalse(Validator.isValidInt("abc"));
        assertFalse(Validator.isValidInt(null));
    }

    @Test
    void testUnitsApi() {
        // Enum toString mapping (stable)
        assertEquals("szt", Validator.Units.SZT.toString());
        assertEquals("kg",  Validator.Units.KG.toString());
        assertEquals("l",   Validator.Units.L.toString());
        assertEquals("m",   Validator.Units.M.toString());
        assertEquals("cm",  Validator.Units.CM.toString());
        assertEquals("mm",  Validator.Units.MM.toString());

        // getAllUnits returns all in declared order
        assertEquals("szt, kg, l, m, cm, mm", Validator.getAllUnits());

        // isValidUnit matches enum toString values (case-sensitive)
        assertTrue(Validator.isValidUnit("szt"));
        assertTrue(Validator.isValidUnit("kg"));
        assertFalse(Validator.isValidUnit("SZT"));
        assertFalse(Validator.isValidUnit("KG"));
        assertFalse(Validator.isValidUnit("pcs"));
        assertFalse(Validator.isValidUnit(null));
    }
}
