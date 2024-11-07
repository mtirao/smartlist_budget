//
//  CreateItem.swift
//  smartlist_budget
//
//  Created by Marcos Tirao on 06/11/2024.
//
import Fluent

struct CreateTender: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema("tenders")
            .id()
            .field("type", .string, .required)
            .field("number", .string, .required)
            .field("alias", .string, .required)
            .field("user_id", .string, .required)
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema("tenders").delete()
    }
}
