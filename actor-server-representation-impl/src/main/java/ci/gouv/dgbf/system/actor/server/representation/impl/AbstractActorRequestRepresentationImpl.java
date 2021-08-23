package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.representation.server.AbstractSpecificRepresentationImpl;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AbstractActorRequest;
import ci.gouv.dgbf.system.actor.server.representation.api.AbstractActorRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AbstractActorRequestDto;

public abstract class AbstractActorRequestRepresentationImpl<REQUEST extends AbstractActorRequestDto,REQUEST_PERSISTENCE extends AbstractActorRequest> extends AbstractSpecificRepresentationImpl<REQUEST> implements AbstractActorRequestRepresentation<REQUEST>,Serializable {
	private static final long serialVersionUID = 1L;

	protected abstract Class<REQUEST_PERSISTENCE> getRequestPersistenceClass();
	
	@Override
	public Response record(List<String> actorsIdentifiers, List<String> requestablesIdentifiers, String actorCode,Boolean ignoreExisting) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __record__(actorsIdentifiers, requestablesIdentifiers, actorCode, ignoreExisting);
					}
				};
			}			
		});
	}
	
	protected abstract TransactionResult __record__(Collection<String> actorsIdentifiers, Collection<String> requestablesIdentifiers, String actorCode,Boolean ignoreExisting);

	@Override
	public Response cancel(List<String> identifiers, String actorCode,Boolean ignoreExisting) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __cancel__(identifiers,actorCode,ignoreExisting);
					}
				};
			}			
		});
	}
	
	protected abstract TransactionResult __cancel__(Collection<String> identifiers, String actorCode,Boolean ignoreExisting);

	@Override
	public Response process(List<REQUEST> requestDtos,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("requestDtos", requestDtos);
						Collection<REQUEST_PERSISTENCE> requests = EntityFinder.getInstance().findMany(getRequestPersistenceClass(), FieldHelper.readSystemIdentifiersAsStrings(requestDtos));
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("requests", requests);
						requests.forEach(actorScopeRequest -> {
							for(REQUEST dto : requestDtos)
								if(actorScopeRequest.getIdentifier().equals(dto.getIdentifier())) {
									actorScopeRequest.setGranted(dto.getGranted());
									actorScopeRequest.setProcessingComment(dto.getProcessingComment());
									break;
								}
						});
						return __process__(requests, actorCode);
					}
				};
			}
		});
	}
	
	protected abstract TransactionResult __process__(Collection<REQUEST_PERSISTENCE> requests,String actorCode);
}