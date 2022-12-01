package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.List;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.representation.server.AbstractSpecificRepresentationImpl.AbstractRunnableImpl;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.RequestScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.representation.api.RequestScopeFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestScopeFunctionDto;

@ApplicationScoped
public class RequestScopeFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<RequestScopeFunctionDto> implements RequestScopeFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(List<String> scopeFunctionsIdentifiers,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scope functions identifiers", scopeFunctionsIdentifiers);
						ThrowableHelper.throwIllegalArgumentExceptionIfBlank("actor code", actorCode);						
						return __inject__(RequestScopeFunctionBusiness.class).updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(scopeFunctionsIdentifiers, actorCode,Boolean.FALSE);
					}
				};
			}
		});
	}
	
	@Override
	public Response notifySignatureSpecimen(List<String> identifiers) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(RequestScopeFunctionBusiness.class).notifySignatureSpecimen(identifiers);
					}
				};
			}
		});
	}
}