package app;

import java.io.IOException;

import com.itextpdf.io.font.PdfEncodings;
import com.itextpdf.kernel.font.PdfFont;
import com.itextpdf.kernel.font.PdfFontFactory;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfString;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.properties.TextAlignment;
import com.itextpdf.layout.properties.UnitValue;

final class PdfInvoiceGenerator {

    private PdfInvoiceGenerator() {
        // Prevent instantiation
    }

    public static void saveToPdf(final String outPath,
        final float vat, final Invoice inv) {
        try (PdfWriter writer = new PdfWriter(outPath);
                PdfDocument pdf = new PdfDocument(writer);
                Document doc = new Document(pdf)) {

            // Set PDF language metadata
            pdf.getCatalog().setLang(new PdfString("pl-PL"));

            // Set a Unicode font supporting Polish diacritics
            // Option A: system font on Linux
            PdfFont font = PdfFontFactory.createFont(
                "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
                PdfEncodings.IDENTITY_H, pdf);

            doc.setFont(font);

            // Title
            doc.add(new Paragraph("Faktura")
                    .setBold()
                    .setFontSize(18)
                    .setMarginBottom(12));

            doc.add(new Paragraph("Data utworzenia: "
                + Formatter.formatDate(inv.getCreationDate()))
                    .setMarginBottom(2));
            doc.add(new Paragraph("Data płatności:  "
                + Formatter.formatDate(inv.getPaymentDate()))
                    .setMarginBottom(8));

            // Parties
            doc.add(new Paragraph("Nabywca:  " + safe(inv.getBuyer()))
                    .setMarginBottom(2));
            doc.add(new Paragraph("Sprzedawca: " + safe(inv.getSeller()))
                    .setMarginBottom(12));

            // Table (auto page break, full width)
            Table table = new Table(UnitValue
                .createPercentArray(new float[] { 30, 10, 15, 10, 20, 15 }))
                    .setWidth(UnitValue.createPercentValue(100));

            // Header
            table.addHeaderCell(new Cell()
                .add(new Paragraph("Produkt").setBold()));
            table.addHeaderCell(new Cell()
                .add(new Paragraph("Jednostka").setBold()));
            table.addHeaderCell(new Cell()
                .add(new Paragraph("Cena jednostkowa").setBold())
                .setTextAlignment(TextAlignment.RIGHT));
            table.addHeaderCell(new Cell()
                .add(new Paragraph("Ilość").setBold())
                .setTextAlignment(TextAlignment.RIGHT));
            table.addHeaderCell(new Cell().add(new Paragraph("Łączna kwota")
                .setBold()).setTextAlignment(TextAlignment.RIGHT));
            table.addHeaderCell(new Cell().add(new Paragraph(
                String.format("VAT(%.2f%%)", vat * 100))
                .setBold()).setTextAlignment(TextAlignment.RIGHT));

            float total = 0f;
            float vatTotal = 0f;
            QuantProduct[] items = inv.getProducts() != null
                ? inv.getProducts() : new QuantProduct[0];

            for (QuantProduct qp : items) {
                Product p = qp.getProduct();
                String name = p != null ? safe(p.getName()) : "-";
                String unit = p != null ? safe(p.getUnit()) : "-";
                float unitPrice = p != null ? p.getCost() : 0f;
                float qty = qp.getQuantity();
                float line = qp.getCumCost();
                float vatPrice = line * vat;
                total += line;
                vatTotal += vatPrice;

                table.addCell(new Paragraph(name));
                table.addCell(new Paragraph(unit));
                table.addCell(new Paragraph(Formatter.formatCurrency(unitPrice))
                    .setTextAlignment(TextAlignment.RIGHT));
                table.addCell(new Paragraph(trimQty(qty))
                    .setTextAlignment(TextAlignment.RIGHT));
                table.addCell(new Paragraph(Formatter.formatCurrency(line))
                    .setTextAlignment(TextAlignment.RIGHT));
                table.addCell(new Paragraph(Formatter.formatCurrency(vatPrice))
                    .setTextAlignment(TextAlignment.RIGHT));
            }

            // Total row
            table.addCell(new Cell(1, 4).add(
                new Paragraph("Łącznie").setBold()));
            table.addCell(new Cell().add(new Paragraph(
                Formatter.formatCurrency(total)).setBold())
                    .setTextAlignment(TextAlignment.RIGHT));
            table.addCell(new Cell().add(new Paragraph(
                Formatter.formatCurrency(vatTotal)).setBold())
                    .setTextAlignment(TextAlignment.RIGHT));

            doc.add(table);

        } catch (IOException e) {
            System.err.println("PDF error: " + e.getMessage());
        }
    }

    private static String safe(final Object o) {
        return o == null ? "-" : o.toString();
    }

    private static String trimQty(final float q) {
        return Math.abs(q - Math.round(q)) < 1e-6
            ? Integer.toString(Math.round(q))
                : String.format("%.2f", q);
    }
}