package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ModulePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Module;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ModulePersistenceImpl extends AbstractPersistenceEntityImpl<Module> implements ModulePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}