//
//  BasketDescriptionController.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Fluent
import Vapor

struct BasketDescriptionController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let tender = routes.grouped("api", apiVersion, "basketdescription")
        
        tender.post(use: self.create)
        tender.group(":basketID") { todo in
            todo.delete(use: self.delete)
        }
        tender.get(use: self.fetch)
    }

    @Sendable
    func fetch(req: Request) async throws -> [BasketDescriptionDTO] {
        guard let userId = req.parameters.get("userID") else { return [] }
        
        let resultBasket = try await Basket.query(on: req.db)
                .filter(\.$userId == userId)
                .group(.or) { group in
                    group.filter(\.$status == .new)
                    group.filter(\.$status == .inprogress)
                }
                .all().map { $0.toDTO() }
        
        guard !resultBasket.isEmpty, let basketId = resultBasket.first!.id  else { return [] }
        
        let result = try await BasketDesscription.query(on: req.db)
                .filter(\.$basketId == basketId)
                .filter(\.$userId == userId)
                .with(\.$item)
                .all().map{ $0.toDTO() }
        
        return result
    }

    @Sendable
    func create(req: Request) async throws -> BasketDescriptionDTO {
        guard let userId = req.parameters.get("userID") else { return BasketDescriptionDTO() }
        
        let todo = try req.content.decode(BasketDescriptionDTO.self).toModel(userId: userId)

        try await todo.save(on: req.db)
        return todo.toDTO()
    }

    @Sendable
    func delete(req: Request) async throws -> HTTPStatus {
        guard let basket = try await BasketDesscription.find(req.parameters.get("basketID"), on: req.db) else {
            throw Abort(.notFound)
        }

        try await basket.delete(on: req.db)
        return .noContent
    }
}

