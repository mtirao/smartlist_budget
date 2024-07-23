//
//  CreateBudget.swift
//  
//
//  Created by Marcos Tirao on 23/07/2024.
//

import Fluent

struct CreateBudget: AsyncMigration {
    func prepare(on database: Database) async throws {
        try await database.schema("budget")
            .id()
            .field("name", .string, .required)
            .field("amount", .double, .required)
            .field("date", .datetime, .required)
            .create()
    }

    func revert(on database: Database) async throws {
        try await database.schema("budget").delete()
    }
}
