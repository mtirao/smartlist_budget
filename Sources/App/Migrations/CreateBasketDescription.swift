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
            .field("date", .date, .required)
            .field("item_id", .uuid, .required)
            .field("basket_id", .uuid, .required)
            .field("user_id", .string, .required)
            .field("price", .double, .required)
            .field("lon", .double, .required)
            .field("lat", .double, .required)
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema("basket_descriptions").delete()
    }
}
