package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.EconomicNaturePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.EconomicNature;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class EconomicNaturePersistenceImpl extends AbstractPersistenceEntityImpl<EconomicNature> implements EconomicNaturePersistence,Serializable {
	private static final long serialVersionUID = 1L;

	
}