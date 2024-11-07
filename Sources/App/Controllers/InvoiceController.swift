//
//  InvoiceController.swift
//
//
//  Created by Marcos Tirao on 29/07/2024.
//

import Fluent
import Vapor

struct InvoiceController: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let invoice = routes.grouped("api", apiVersion, "invoice")

        invoice.get(use: self.index)
        invoice.post(use: self.create)
        invoice.group(":invoiceID") { todo in
            invoice.delete(use: self.delete)
        }
    }

    @Sendable
    func index(req: Request) async throws -> [InvoiceDTO] {
        return try await Invoice.query(on: req.db).all().map { $0.toDTO() }
    }

    @Sendable
    func create(req: Request) async throws -> InvoiceDTO {
        guard let userId = req.parameters.get("userID") else { return InvoiceDTO() }
        
        let todo = try req.content.decode(InvoiceDTO.self).toModel(userId: userId)

        try await todo.save(on: req.db)
        return todo.toDTO()
    }

    @Sendable
    func delete(req: Request) async throws -> HTTPStatus {
        guard let invoice = try await Invoice.find(req.parameters.get("invoiceID"), on: req.db) else {
            throw Abort(.notFound)
        }

        try await invoice.delete(on: req.db)
        return .noContent
    }
}
