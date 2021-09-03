package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.map.CollectionOfMapsStringStringBuilder;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.representation.server.AbstractSpecificRepresentationImpl.AbstractRunnableImpl;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeTypeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.representation.api.ScopeTypeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeTypeDto;

@ApplicationScoped
public class ScopeTypeRepresentationImpl extends AbstractRepresentationEntityImpl<ScopeTypeDto> implements ScopeTypeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response create(String code, String name, Byte orderNumber, Boolean requestable,String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __inject__(ScopeTypeBusiness.class).create(code, name, orderNumber, requestable, actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response update(String identifier, String code, String name, Byte orderNumber,Boolean requestable, String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __inject__(ScopeTypeBusiness.class).update(identifier,code, name, orderNumber, requestable, actorCode);
					}
				};
			}
		});
	}
	
	@Override
	public Response save(String identifier, String code, String name, Byte orderNumber,Boolean requestable, String actorCode) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new AbstractRunnableImpl.TransactionImpl(responseBuilderArguments){
					@Override
					public TransactionResult transact() {
						return __inject__(ScopeTypeBusiness.class).save(identifier,code, name, orderNumber, requestable, actorCode);
					}
				};
			}
		});
	}
	
	public static Response get(Boolean requestable,Boolean pageable,Integer firstTupleIndex,Integer numberOfTuples) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runnable getRunnable() {
				return new Runnable() {
					@Override
					public void run() {
						Collection<ScopeType> profileTypes = __inject__(ScopeTypeBusiness.class).get(requestable,pageable, firstTupleIndex, numberOfTuples);
						CollectionOfMapsStringStringBuilder.Arguments<ScopeType> arguments = new CollectionOfMapsStringStringBuilder.Arguments<ScopeType>()
								.setCollection(profileTypes).addFieldsNames(ScopeType.FIELD_IDENTIFIER,ScopeTypeDto.JSON_FIELD_IDENTIFIER
										,ScopeType.FIELD_CODE,ScopeTypeDto.JSON_FIELD_CODE,ScopeType.FIELD_NAME,ScopeTypeDto.JSON_FIELD_NAME);
						arguments.setEmptyValueAsArrayList();
						responseBuilderArguments.setEntity(CollectionOfMapsStringStringBuilder.getInstance().build(ScopeType.class, arguments));
					}
				};
			}
		});
	}
}