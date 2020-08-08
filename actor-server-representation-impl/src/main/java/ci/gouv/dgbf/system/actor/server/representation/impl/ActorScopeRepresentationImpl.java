package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
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
	public Response createByScopes(Collection<ScopeDto> scopes) {
		if(CollectionHelper.isEmpty(scopes))
			return null;
		ActorDto actor = null;
		for(ScopeDto index : scopes)
			if(index.getActor() != null) {
				actor = index.getActor();
				break;
			}
		return createByActorByScopes(actor, scopes);
	}
	
	@Override
	public Response createByActorByScopes(ActorDto actor, Collection<ScopeDto> scopes) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {				
					@Override
					public void run() {
						Actor __actor__ = EntityFinder.getInstance().find(Actor.class, actor.getIdentifier());
						Collection<Scope> __scopes__ = null;
						for(ScopeDto index : scopes) {
							Scope scope = EntityFinder.getInstance().find(Scope.class, index.getIdentifier());
							if(scope != null) {
								if(__scopes__ == null)
									__scopes__ = new ArrayList<>();
								__scopes__.add(scope);
							}
						}
						__inject__(ActorScopeBusiness.class).createByActorByScopes(__actor__, __scopes__);
					}
				};
			}			
		});
	}
	
	@Override
	public Response deleteByActorByScopes(ActorDto actor, Collection<ScopeDto> scopes) {
		if(actor == null)
			return Response.serverError().entity("Actor required").build();
		if(CollectionHelper.isEmpty(scopes))
			return Response.serverError().entity("Scopes required").build();
		Actor __actor__ = EntityFinder.getInstance().find(Actor.class, actor.getCode());
		Collection<Scope> __scopes__ = new ArrayList<>();
		scopes.forEach(scopeDto -> {
			__scopes__.add(EntityFinder.getInstance().find(Scope.class, scopeDto.getCode()));
		});
		Runner.Arguments runnerArguments = new Runner.Arguments().addRunnables(new Runnable() {				
			@Override
			public void run() {
				__inject__(ActorScopeBusiness.class).deleteByActorByScopes(__actor__, __scopes__);
			}
		});
		Runner.getInstance().run(runnerArguments);
		if(runnerArguments.getThrowable() == null)
			return Response.ok(/*ProfilePrivileges.size()+" funding source lessors has been processed"*/).build();
		return ResponseBuilder.getInstance().build(runnerArguments.getThrowable());
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