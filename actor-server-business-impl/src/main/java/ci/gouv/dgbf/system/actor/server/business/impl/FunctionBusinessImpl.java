package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;
import org.cyk.utility.server.business.BusinessFunctionCreator;
import org.cyk.utility.server.business.BusinessFunctionModifier;

import ci.gouv.dgbf.system.actor.server.business.api.FunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.FunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;

@ApplicationScoped
public class FunctionBusinessImpl extends AbstractBusinessEntityImpl<Function, FunctionPersistence> implements FunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	protected void __listenExecuteCreateBefore__(Function function_, Properties properties,BusinessFunctionCreator function) {
		super.__listenExecuteCreateBefore__(function_, properties, function);
		__setNumberOfActorPerScope__(function_);
	}
	
	@Override
	protected void __listenExecuteUpdateBefore__(Function function_, Properties properties,BusinessFunctionModifier function) {
		super.__listenExecuteUpdateBefore__(function_, properties, function);
		__setNumberOfActorPerScope__(function_);
	}
	
	private void __setNumberOfActorPerScope__(Function function) {
		if(Boolean.TRUE.equals(function.getShared())) {
			function.setNumberOfActorPerScope(0);
		}else {
			function.setNumberOfActorPerScope(1);
		}
	}
	
}