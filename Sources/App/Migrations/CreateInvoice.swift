//
//  CreateInvoice.swift
//
//
//  Created by Marcos Tirao on 29/07/2024.
//

import Fluent


struct CreateInvoice: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema("invoices")
            .id()
            .field("amount", .double, .required)
            .field("budget", .uuid, .required)
            .field("date", .date, .required)
            .field("name", .string, .required)
            .field("user_id", .string, .required)
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema("invoices").delete()
    }
}
