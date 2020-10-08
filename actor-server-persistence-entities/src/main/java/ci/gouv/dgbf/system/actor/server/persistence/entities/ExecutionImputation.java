package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Table;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ExecutionImputation.TABLE_NAME)
public class ExecutionImputation extends AbstractImputation implements Serializable {
	private static final long serialVersionUID = 1L;
	
	public static final String TABLE_NAME = "VM_APP_EX_IMPUTATION";
}