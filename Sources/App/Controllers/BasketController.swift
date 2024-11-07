//
//  BasketController.swift
//
//
//  Created by Marcos Tirao on 15/08/2024.
//

import Fluent
import Vapor

struct BasketController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let tender = routes.grouped("api", apiVersion, "basket")
        
        tender.post(use: self.create)
        tender.group(":basketID") { todo in
            todo.delete(use: self.delete)
        }
        tender.get(use: self.fetch)
    }

    @Sendable
    func fetch(req: Request) async throws -> [BasketDTO] {
        guard let userId = req.parameters.get("userID") else { return [] }
        
        let result = try await Basket.query(on: req.db).filter(\.$userId == userId).all().map { $0.toDTO() }
        if result.isEmpty {
            return []
        }
        
        return result
    }

    @Sendable
    func create(req: Request) async throws -> HTTPStatus {
        guard let userId = req.parameters.get("userID") else { throw Abort(.nonAuthoritativeInformation) }
        
        let item = try req.content.decode(ItemDTO.self).toModel(userId: userId)
        
        let result = try await Basket.query(on: req.db)
                .filter(\.$userId == userId)
                .group(.or) { group in
                    group.filter(\.$status == .new)
                    group.filter(\.$status == .inprogress)
                }
                .all()
        
        if result.isEmpty {
            let uuid = UUID()
            let basket = BasketDTO(id: uuid, status: .new).toModel(userId: userId)
            let basketDescription = BasketDescriptionDTO(
                itemId: item.id,
                basketId: uuid,
                price: 0,
                lon: 0,
                lat: 0).toModel(userId: userId)
            
            try await req.db.transaction { database in
                try await basket.save(on: database)
                try await basketDescription.save(on: database)
            }
            
            return .noContent
        } else {
            let basket = result.first!
            let basketDescription = BasketDescriptionDTO(
                itemId: item.id,
                basketId: basket.id,
                price: 0,
                lon: 0,
                lat: 0).toModel(userId: userId)
            try await basketDescription.save(on: req.db)
        }
        
        return .noContent
    }

    @Sendable
    func delete(req: Request) async throws -> HTTPStatus {
        guard let basket = try await Basket.find(req.parameters.get("basketID"), on: req.db) else {
            throw Abort(.notFound)
        }

        try await basket.delete(on: req.db)
        return .noContent
    }
}

