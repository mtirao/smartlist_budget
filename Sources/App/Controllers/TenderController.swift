//
//  TenderController.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Fluent
import Vapor

struct TenderController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let tender = routes.grouped("api", apiVersion, "tender")

        tender.get(use: self.index)
        tender.post(use: self.create)
        tender.group(":tenderID") { todo in
            tender.delete(use: self.delete)
        }
    }

    @Sendable
    func index(req: Request) async throws -> [TenderDTO] {
        return try await Tender.query(on: req.db).all().map { $0.toDTO() }
    }

    @Sendable
    func create(req: Request) async throws -> TenderDTO {
        let todo = try req.content.decode(TenderDTO.self).toModel()

        try await todo.save(on: req.db)
        return todo.toDTO()
    }

    @Sendable
    func delete(req: Request) async throws -> HTTPStatus {
        guard let tender = try await Tender.find(req.parameters.get("tenderID"), on: req.db) else {
            throw Abort(.notFound)
        }

        try await tender.delete(on: req.db)
        return .noContent
    }
}

