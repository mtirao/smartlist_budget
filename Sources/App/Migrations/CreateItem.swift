//
//  CreateItem.swift
//  smartlist_budget
//
//  Created by Marcos Tirao on 06/11/2024.
//
import Fluent

struct CreateItem: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema("items")
            .id()
            .field("category", .string, .required)
            .field("name", .string, .required)
            .field("sku", .string, .required)
            .field("user_id", .string, .required)
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema("items").delete()
    }
}
