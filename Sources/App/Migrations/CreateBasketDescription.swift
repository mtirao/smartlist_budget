//
//  CreateBasketDescription.swift
//  smartlist_budget
//
//  Created by Marcos Tirao on 06/11/2024.
//
import Fluent


struct CreateBasketDescription: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema("basket_descriptions")
            .id()
            .field("date", .date)
            .field("item_id", .uuid, .required, .references("items", "id"))
            .field("basket_id", .uuid, .required)
            .field("user_id", .string, .required)
            .field("price", .double, .required)
            .field("lon", .double)
            .field("lat", .double)
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema("basket_descriptions").delete()
    }
}
