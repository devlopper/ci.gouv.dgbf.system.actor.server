package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.representation.api.ScopeFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeFunctionDto;

@ApplicationScoped
public class ScopeFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<ScopeFunctionDto> implements ScopeFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response save(Collection<ScopeFunctionDto> scopeFunctionDtos) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scopeFunctionDtos", scopeFunctionDtos);
						Collection<ScopeFunction> scopeFunctions = null;
						for(ScopeFunctionDto scopeFunctionDto : scopeFunctionDtos) {
							ScopeFunction scopeFunction = null;
							if(StringHelper.isBlank(scopeFunctionDto.getIdentifier())) {
								scopeFunction = MappingHelper.getDestination(scopeFunctionDto, ScopeFunction.class);
							}else {
								scopeFunction = EntityFinder.getInstance().find(ScopeFunction.class, scopeFunctionDto.getIdentifier());
								if(scopeFunction != null) {
									if(scopeFunctionDto.getScope() == null)
										scopeFunction.setScope(null);
									else {
										if(!scopeFunction.getScope().getIdentifier().equals(scopeFunctionDto.getScope().getIdentifier()))
											scopeFunction.setScope(EntityFinder.getInstance().find(Scope.class, scopeFunctionDto.getScope().getIdentifier()));
									}
									
									if(scopeFunctionDto.getFunction() == null)
										scopeFunction.setFunction(null);
									else {
										if(!scopeFunction.getFunction().getIdentifier().equals(scopeFunctionDto.getFunction().getIdentifier()))
											scopeFunction.setFunction(EntityFinder.getInstance().find(Function.class, scopeFunctionDto.getFunction().getIdentifier()));
									}
									if(StringHelper.isNotBlank(scopeFunctionDto.getCode()))
										scopeFunction.setCode(scopeFunctionDto.getCode());
									if(StringHelper.isNotBlank(scopeFunctionDto.getName()))
										scopeFunction.setName(scopeFunctionDto.getName());
									scopeFunction.setShared(scopeFunctionDto.getShared());
								}
							}
							if(scopeFunction == null)
								continue;
							if(scopeFunctions == null)
								scopeFunctions = new ArrayList<>();
							scopeFunction.set__auditWho__(scopeFunctionDto.get__auditWho__());
							scopeFunction.set__auditFunctionality__(scopeFunctionDto.get__auditFunctionality__());
							scopeFunctions.add(scopeFunction);
						}
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scopeFunctions", scopeFunctions);
						__inject__(ScopeFunctionBusiness.class).saveMany(scopeFunctions);
					}
				};
			}
		});
	}
	
	@Override
	public Response deriveAll() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionBusiness.class).deriveAll();
					}
				};
			}
		});
	}
	
	@Override
	public Response deriveByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionBusiness.class).deriveByFunctionsIdentifiers(functionsIdentifiers);
					}
				};
			}
		});
	}
	
	@Override
	public Response deriveHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionBusiness.class).deriveHoldersAndAssistantsByHoldersFunctionsIdentifiers(holdersFunctionsIdentifiers);
					}
				};
			}
		});
	}
	
	@Override
	public Response codifyAll() {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionBusiness.class).codifyAll();
					}
				};
			}
		});
	}
	
	@Override
	public Response codifyByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionBusiness.class).codifyByFunctionsIdentifiers(functionsIdentifiers);
					}
				};
			}
		});
	}
	
	@Override
	public Response codifyHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionBusiness.class).codifyHoldersAndAssistantsByHoldersFunctionsIdentifiers(holdersFunctionsIdentifiers);
					}
				};
			}
		});
	}
	
	@Override
	public Response deleteByFunctionsIdentifiers(Collection<String> functionsIdentifiers) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionBusiness.class).deleteByFunctionsIdentifiers(functionsIdentifiers);
					}
				};
			}
		});
	}
	
	@Override
	public Response deleteHoldersAndAssistantsByHoldersFunctionsIdentifiers(Collection<String> holdersFunctionsIdentifiers) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						__inject__(ScopeFunctionBusiness.class).deleteHoldersAndAssistantsByHoldersFunctionsIdentifiers(holdersFunctionsIdentifiers);
					}
				};
			}
		});
	}
}
