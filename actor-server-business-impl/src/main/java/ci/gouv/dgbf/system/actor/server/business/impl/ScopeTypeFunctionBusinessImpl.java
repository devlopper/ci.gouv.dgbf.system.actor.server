package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeTypeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeTypeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ScopeTypeFunctionBusinessImpl extends AbstractBusinessEntityImpl<ScopeTypeFunction, ScopeTypeFunctionPersistence> implements ScopeTypeFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
