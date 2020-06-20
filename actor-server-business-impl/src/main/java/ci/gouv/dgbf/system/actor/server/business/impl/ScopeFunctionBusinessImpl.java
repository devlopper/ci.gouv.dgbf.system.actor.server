package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ScopeFunctionBusinessImpl extends AbstractBusinessEntityImpl<ScopeFunction, ScopeFunctionPersistence> implements ScopeFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
