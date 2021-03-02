package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.business.server.EntitySaver;
import org.cyk.utility.business.server.EntitySaver.Arguments;
import org.cyk.utility.__kernel__.mapping.MappingHelper;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;
import ci.gouv.dgbf.system.actor.server.representation.api.ScopeTypeFunctionRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeTypeFunctionDto;

@ApplicationScoped
public class ScopeTypeFunctionRepresentationImpl extends AbstractRepresentationEntityImpl<ScopeTypeFunctionDto> implements ScopeTypeFunctionRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response save(Collection<ScopeTypeFunctionDto> scopeTypeFunctionDtos) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scopeTypeFunctions", scopeTypeFunctionDtos);
						Collection<ScopeTypeFunction> creatables = new ArrayList<>();
						Collection<ScopeTypeFunction> updatables = new ArrayList<>();
						Collection<ScopeTypeFunction> deletables = new ArrayList<>();		
						for(ScopeTypeFunctionDto index : scopeTypeFunctionDtos) {
							ScopeTypeFunction scopeTypeFunction = MappingHelper.getDestination(index, ScopeTypeFunction.class);
							if(StringHelper.isBlank(index.getIdentifier()))
								creatables.add(scopeTypeFunction);
							else if(Boolean.TRUE.equals(index.get__deletable__()))
								deletables.add(scopeTypeFunction);
							else
								updatables.add(scopeTypeFunction);
						}
						
						Arguments<ScopeTypeFunction> arguments = new Arguments<ScopeTypeFunction>();
						arguments.setPersistenceArguments(new org.cyk.utility.persistence.query.EntitySaver.Arguments<ScopeTypeFunction>().setCreatables(creatables).setUpdatables(updatables).setDeletables(deletables));
						EntitySaver.getInstance().save(ScopeTypeFunction.class, arguments);
					}
				};
			}
		});
	}	
}