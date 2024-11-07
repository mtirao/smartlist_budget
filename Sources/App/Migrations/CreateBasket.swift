//
//  CreateBasket.swift
//  smartlist_budget
//
//  Created by Marcos Tirao on 06/11/2024.
//
import Fluent


struct CreateBasket: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema("baskets")
            .id()
            .field("status", .string, .required)
            .field("user_id", .string, .required)
            .field("tender_id", .uuid)
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema("baskets").delete()
    }
}
