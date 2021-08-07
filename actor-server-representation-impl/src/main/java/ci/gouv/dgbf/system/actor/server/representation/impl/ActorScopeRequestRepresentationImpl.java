package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.representation.server.AbstractSpecificRepresentationImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ActorScopeRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;
import ci.gouv.dgbf.system.actor.server.representation.api.ActorScopeRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ActorScopeRequestDto;

@ApplicationScoped
public class ActorScopeRequestRepresentationImpl extends AbstractSpecificRepresentationImpl<ActorScopeRequestDto> implements ActorScopeRequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response record(List<String> actorsIdentifiers, List<String> scopesIdentifiers, String actorCode,Boolean ignoreExisting) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __inject__(ActorScopeRequestBusiness.class).record(actorsIdentifiers, scopesIdentifiers, actorCode, ignoreExisting);
					}
				};
			}			
		});
	}

	@Override
	public Response cancel(List<String> identifiers, String actorCode,Boolean ignoreExisting) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __inject__(ActorScopeRequestBusiness.class).cancel(identifiers,actorCode,ignoreExisting);
					}
				};
			}			
		});
	}

	@Override
	public Response process(List<ActorScopeRequestDto> actorScopeRequestDtos,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("actorScopeRequestDtos", actorScopeRequestDtos);
						Collection<ActorScopeRequest> actorScopeRequests = EntityFinder.getInstance().findMany(ActorScopeRequest.class, FieldHelper.readSystemIdentifiersAsStrings(actorScopeRequestDtos));
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("actorScopeRequests", actorScopeRequests);
						actorScopeRequests.forEach(actorScopeRequest -> {
							for(ActorScopeRequestDto dto : actorScopeRequestDtos)
								if(actorScopeRequest.getIdentifier().equals(dto.getIdentifier())) {
									actorScopeRequest.setGranted(dto.getGranted());
									actorScopeRequest.setProcessingComment(dto.getProcessingComment());
									break;
								}
						});
						return __inject__(ActorScopeRequestBusiness.class).process(actorScopeRequests, actorCode);
					}
				};
			}			
		});
	}
}