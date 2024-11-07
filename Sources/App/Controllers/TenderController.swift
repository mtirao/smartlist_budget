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
        tender.post(use: self.create)
        tender.group(":tenderID") { todo in
            todo.delete(use: self.delete)
        }
        tender.get(use: self.fetch)
    }

    @Sendable
    func fetch(req: Request) async throws -> [TenderDTO] {
        guard let userId = req.parameters.get("userID") else { return [] }
        
        let result = try await Tender.query(on: req.db).filter(\.$userId == userId).all().map { $0.toDTO() }
        if result.isEmpty {
            return []
        }
        
        return result
    }

    @Sendable
    func create(req: Request) async throws -> TenderDTO {
        guard let userId = req.parameters.get("userID") else { return TenderDTO() }
        
        let todo = try req.content.decode(TenderDTO.self).toModel(userId: userId)

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

