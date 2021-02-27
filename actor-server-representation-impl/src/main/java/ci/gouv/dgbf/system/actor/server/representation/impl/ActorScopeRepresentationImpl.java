package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.rest.ResponseBuilder;
import org.cyk.utility.__kernel__.runnable.Runner;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ActorScopeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorScopeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorScopeDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeDto;

@ApplicationScoped
public class ActorScopeRepresentationImpl extends AbstractRepresentationEntityImpl<ActorScopeDto> implements ActorScopeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response createByActorsByScopes(Collection<ActorDto> actors, Collection<ScopeDto> scopes) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {				
					@Override
					public void run() {
						__inject__(ActorScopeBusiness.class).createByActorsByScopes(
								EntityFinder.getInstance().findMany(Actor.class, FieldHelper.readSystemIdentifiersAsStrings(actors))
								, EntityFinder.getInstance().findMany(Scope.class, FieldHelper.readSystemIdentifiersAsStrings(scopes)));
					}
				};
			}			
		});
	}
	
	@Override
	public Response createByActors(Collection<ActorDto> actors) {
		Collection<ScopeDto> scopes = null;
		if(CollectionHelper.isNotEmpty(actors))
			for(ActorDto actor : actors)
				if(scopes == null && CollectionHelper.isNotEmpty(actor.getScopesIdentifiers())) {
					scopes = actor.getScopesIdentifiers().stream().map(scopeIdentifier -> new ScopeDto().setIdentifier(scopeIdentifier)).collect(Collectors.toList());
					break;
				}
		return createByActorsByScopes(actors, scopes);
	}
	
	@Override
	public Response deleteByActorsByScopes(Collection<ActorDto> actors, Collection<ScopeDto> scopes) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {				
					@Override
					public void run() {
						__inject__(ActorScopeBusiness.class).deleteByActorsByScopes(
								EntityFinder.getInstance().findMany(Actor.class, FieldHelper.readSystemIdentifiersAsStrings(actors))
								, EntityFinder.getInstance().findMany(Scope.class, FieldHelper.readSystemIdentifiersAsStrings(scopes)));
					}
				};
			}			
		});
	}
	
	@Override
	public Response deleteByActors(Collection<ActorDto> actors) {
		Collection<ScopeDto> scopes = null;
		if(CollectionHelper.isNotEmpty(actors))
			for(ActorDto actor : actors)
				if(scopes == null && CollectionHelper.isNotEmpty(actor.getScopesIdentifiers())) {
					scopes = actor.getScopesIdentifiers().stream().map(scopeIdentifier -> new ScopeDto().setIdentifier(scopeIdentifier)).collect(Collectors.toList());
					break;
				}
		return deleteByActorsByScopes(actors, scopes);
	}
	
	@Override
	public Response createByScopes(Collection<ScopeDto> scopes) {
		if(CollectionHelper.isEmpty(scopes))
			return null;
		ActorDto actor = null;
		for(ScopeDto index : scopes)
			if(index.getActor() != null) {
				actor = index.getActor();
				break;
			}
		return createByActorsByScopes(List.of(actor), scopes);
	}
	
	@Override
	public Response deleteByScopes(Collection<ScopeDto> scopes) {
		if(CollectionHelper.isEmpty(scopes))
			return Response.serverError().entity("Scopes are required").build();
		Actor __actor__ = null;
		Collection<Scope> __scopes__ = new ArrayList<>();
		for(ScopeDto scopeDto : scopes) {
			if(scopeDto.getActor() != null)
				__actor__ = EntityFinder.getInstance().find(Actor.class, scopeDto.getActor().getIdentifier());
			__scopes__.add(EntityFinder.getInstance().find(Scope.class, scopeDto.getIdentifier()));
		}
		Actor a = __actor__;
		Runner.Arguments runnerArguments = new Runner.Arguments().addRunnables(new Runnable() {				
			@Override
			public void run() {
				__inject__(ActorScopeBusiness.class).deleteByActorByScopes(a, __scopes__);
			}
		});
		Runner.getInstance().run(runnerArguments);
		if(runnerArguments.getThrowable() == null)
			return Response.ok(/*ProfilePrivileges.size()+" funding source lessors has been processed"*/).build();
		return ResponseBuilder.getInstance().build(runnerArguments.getThrowable());
	}
}